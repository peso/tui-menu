/*! Simple menu with Turbo Vision looks.

This menu is modelled after the Turbo Vision api.
The visuals are also aligned.

## Main structs

As dictated by Ratatui immediate mode, we model visual elements
in a Widget and a WidgetState part. The first holds the configuration,
and never changes. The second holds the parts that can be affected
by user actions.

In this case the style is found in the Widget [Menu]
And menu items, as well as which is currently selected, are found in
WidgetState [MenuState], which is built from [MenuItem].
Any generated events (triggering a menu item) will be stored
in MenuState.events.

To define a menu, see examples in [MenuState].
*/

#![allow(dead_code)]

use ratatui::{
    crossterm::event::{MouseButton, MouseEvent, MouseEventKind},
    layout::Rect,
    prelude::Position,
    style::{Color, Style, Styled},
    text::{Line, Span},
    widgets::{Clear, StatefulWidget, Widget},
};
use std::{borrow::Cow, cell::Ref, cell::RefCell, cell::RefMut, marker::PhantomData, rc::Rc};

#[derive(Clone, Eq, PartialEq, Debug)]
/// Events this widget consume. Commands to the menu
/// Your application should map key events to these actions
pub enum MenuAction {
    // menu state
    Activate,
    Reset,

    // navigate
    Up,
    Down,
    Left,
    Right,

    // activate current menu item
    Select,
}


/// Events this widget produce
/// Now only emit Selected, may add few in future
#[derive(Debug)]
pub enum MenuEvent<T> {
    /// Item selected, with its data attached
    Selected(T),
}

pub struct RefMenuItem<T>(Rc<RefCell<MenuItem<T>>>);

impl<T> Clone for RefMenuItem<T> {
    fn clone(&self) -> Self {
        Self {
            0: Rc::clone(&self.0),
        }
    }
}

impl<T> RefMenuItem<T> {
    // Wrap MenuItem in a RefMenuItem
    pub fn new(menu_item: MenuItem<T>) -> Self {
        Self {
            0: Rc::new(RefCell::new(menu_item)),
        }
    }

    pub fn borrow(&self) -> Ref<MenuItem<T>> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<MenuItem<T>> {
        self.0.borrow_mut()
    }

    // Move highlight to previous child
    pub fn highlight_prev(&self) {
        self.borrow_mut().highlight_prev()
    }

    // Move highlight to next child
    pub fn highlight_next(&self) {
        self.borrow_mut().highlight_next()
    }

    // Return index of highlighted child
    pub fn highlight_child_index(&self) -> Option<usize> {
        self.borrow().highlight_child_index()
    }

    // Return ref to highlighted child
    pub fn highlight_child(&self) -> Option<RefMenuItem<T>> {
        let Some(rmi) = self.borrow().highlight_child() else {
            return None;
        };
        Some(rmi.clone())
    }

    // Clear all highlights so menu is foldet up
    pub fn clear_highlight(&self) {
        self.borrow_mut().clear_highlight()
    }

    fn find_menu_item(&self, point: Position) -> Option<(RefMenuItem<T>, usize)> {
        find_menu_item(self, point)
    }
}

/// Find the deepest child that contains the point
/// Return parent and index into parent
/// Does not test this menu item.
fn find_menu_item<T>(parent: &RefMenuItem<T>, point: Position) -> Option<(RefMenuItem<T>, usize)> {
    // Check grandchildren first
    for child in parent.borrow().children.iter() {
        if child.borrow().is_highlight {
            let result = child.find_menu_item(point);
            if result.is_some() {
                return result.clone();
            }
        }
    }
    // Check all children
    for (inx, child_rect) in parent.borrow().child_rect.iter().enumerate() {
        if child_rect.contains(point) {
            return Some((parent.clone(), inx));
        }
    }
    return None;
}

/// The state for menu, keep track of runtime info
pub struct MenuState<T> {
    /// stores the menu tree
    root_item: RefMenuItem<T>,
    /// stores events generated in one frame
    events: Vec<MenuEvent<T>>,
}

impl<T: Clone> MenuState<T> {
    /// create with items
    /// # Example
    ///
    /// ```
    /// use tui_menu::{MenuState, MenuItem};
    ///
    /// let state = MenuState::<&'static str>::new(vec![
    ///     MenuItem::item("Foo", "label_foo"),
    ///     MenuItem::group("Group", vec![
    ///         MenuItem::item("Bar 1", "label_bar_1"),
    ///         MenuItem::item("Bar 2", "label_bar_1"),
    ///     ])
    /// ]);
    /// ```
    pub fn new(items: Vec<MenuItem<T>>) -> Self {
        let root_item = RefMenuItem::new(MenuItem::group("root", items));
        // the root item marked as always highlight
        // this makes highlight logic more consistent
        root_item.borrow_mut().is_highlight = true;
        // BUG I think it is a bug
        root_item.borrow_mut().is_highlight = false;

        Self {
            root_item,
            events: Default::default(),
        }
    }

    /// active the menu, this will select the first item
    ///
    /// # Example
    ///
    /// ```
    /// use tui_menu::{MenuState, MenuItem};
    ///
    /// let mut state = MenuState::<&'static str>::new(vec![
    ///     MenuItem::item("Foo", "label_foo"),
    ///     MenuItem::group("Group", vec![
    ///         MenuItem::item("Bar 1", "label_bar_1"),
    ///         MenuItem::item("Bar 2", "label_bar_1"),
    ///     ])
    /// ]);
    ///
    /// state.activate();
    ///
    /// assert_eq!(state.find_highlight().unwrap().data.unwrap(), "label_foo");
    ///
    /// ```
    ///
    pub fn activate(&mut self) {
        if self.root_item.borrow().is_highlight {
            return;
        }
        self.root_item.borrow_mut().is_highlight = true;
        self.root_item.highlight_next();
    }

    /// Check if menu is active
    pub fn is_active(&self) -> bool {
        return self.find_highlight().is_some();
    }

    /// trigger up movement
    /// NOTE: this action tries to do intuitive movement,
    /// which means logically it is not consistent, e.g:
    /// case 1:
    ///    group 1        group 2        group 3
    ///                 > sub item 1
    ///                   sub item 2
    /// up is pop, which closes the group 2
    ///
    /// case 2:
    ///    group 1        group 2        group 3
    ///                   sub item 1
    ///                 > sub item 2
    /// up is move prev
    ///
    /// case 3:
    ///
    ///    group 1        group 2   
    ///                   sub item 1
    ///                 > sub item 2  > sub sub item 1
    ///                                 sub sub item 2
    ///
    /// up does nothing
    pub fn up(&mut self) {
        match self.active_depth() {
            0 | 1 => {
                // do nothing
            }
            2 => match self
                .root_item
                .highlight_child()
                .and_then(|child| child.highlight_child_index())
            {
                // case 1
                Some(0) => {
                    self.pop();
                }
                _ => {
                    self.prev();
                }
            },
            _ => {
                self.prev();
            }
        }
    }

    /// trigger down movement
    ///
    /// NOTE: this action tries to do intuitive movement,
    /// which means logicially it is not consistent, e.g:
    /// case 1:
    ///    group 1      > group 2        group 3
    ///                   sub item 1
    ///                   sub item 2
    /// down is enter, which enter the sub group of group 2
    ///
    /// case 2:
    ///    group 1        group 2        group 3
    ///                   sub item 1
    ///                 > sub item 2
    /// down does nothing
    ///
    /// case 3:
    ///    group 1        group 2   
    ///                 > sub item 1
    ///                   sub item 2
    ///
    /// down highlights "sub item 2"
    pub fn down(&mut self) {
        if self.active_depth() == 1 {
            self.push();
        } else {
            self.next();
        }
    }

    /// trigger left movement
    ///
    /// NOTE: this action tries to do intuitive movement,
    /// which means logicially it is not consistent, e.g:
    /// case 1:
    ///    group 1      > group 2        group 3
    ///                   sub item 1
    ///                   sub item 2
    /// left highlights "group 1"
    ///
    /// case 2:
    ///    group 1        group 2        group 3
    ///                   sub item 1
    ///                 > sub item 2
    /// left first pop "sub item group", then highlights "group 1"
    ///
    /// case 3:
    ///    group 1        group 2   
    ///                 > sub item 1    sub sub item 1
    ///                   sub item 2  > sub sub item 2
    ///
    /// left pop "sub sub group"
    pub fn left(&mut self) {
        if self.active_depth() == 0 {
            // do nothing
        } else if self.active_depth() == 1 {
            self.prev();
        } else if self.active_depth() == 2 {
            self.pop();
            self.prev();
        } else {
            self.pop();
        }
    }

    /// trigger right movement
    ///
    /// NOTE: this action tries to do intuitive movement,
    /// which means logicially it is not consistent, e.g:
    /// case 1:
    ///    group 1      > group 2        group 3
    ///                   sub item 1
    ///                   sub item 2
    /// right highlights "group 3"
    ///
    /// case 2:
    ///    group 1        group 2        group 3
    ///                   sub item 1
    ///                 > sub item 2
    /// right pop group "sub item *", then highlights "group 3"
    ///
    /// case 3:
    ///    group 1        group 2        group 3
    ///                   sub item 1
    ///                 > sub item 2 +
    /// right pushes "sub sub item 2". this differs from case 2 that
    /// current highlighted item can be expanded
    pub fn right(&mut self) {
        if self.active_depth() == 0 {
            // do nothing
        } else if self.active_depth() == 1 {
            self.next();
        } else if self.active_depth() == 2 {
            if self.push().is_none() {
                // special handling, make menu navigation
                // more productive
                self.pop();
                self.next();
            }
        } else {
            self.push();
        }
    }

    /// highlight the prev item in current group
    /// if already the first, then do nothing
    fn prev(&mut self) {
        if let Some(item) = self.find_highlight_parent() {
            item.highlight_prev();
        } else {
            self.root_item.highlight_prev();
        }
    }

    /// highlight the next item in current group
    /// if already the last, then do nothing
    fn next(&mut self) {
        if let Some(item) = self.find_highlight_parent() {
            item.highlight_next();
        } else {
            self.root_item.highlight_next();
        }
    }

    /// active depth, how many levels dropdown/sub menus expanded.
    /// when no drop down, it is 1
    /// one drop down, 2
    fn active_depth(&self) -> usize {
        let mut item = self.root_item.highlight_child();
        let mut depth = 0;
        while let Some(inner_item) = item {
            depth += 1;
            item = inner_item.highlight_child();
        }
        depth
    }

    /// How many dropdown to render, including preview
    /// NOTE: If current group contains sub-group, in order to keep ui consistent,
    ///   even the sub-group not selected, its space is counted
    fn dropdown_count(&self) -> u16 {
        let mut node: RefMenuItem<T> = self.root_item.clone();
        let mut count = 0;
        loop {
            match node.highlight_child() {
                None => {
                    return count;
                }
                Some(highlight_child) => {
                    if highlight_child.borrow().is_group() {
                        // highlighted child is a group, then it's children is previewed
                        count += 1;
                    } else if node.borrow().children.iter().any(|c| c.borrow().is_group()) {
                        // if highlighted item is not a group, but if sibling contains group
                        // in order to keep ui consistency, also count it
                        count += 1;
                    }

                    node = highlight_child.clone();
                }
            }
        }
    }

    /// select current highlight item, if it has children
    /// then push
    pub fn select(&mut self) {
        if let Some(item) = self.find_highlight() {
            if !item.borrow().children.is_empty() {
                self.push();
            } else if !item.borrow().enabled {
                return; // Ignore disabled items
            } else if let Some(ref data) = item.borrow().data {
                self.events.push(MenuEvent::Selected(data.clone()));
                // TODO self.reset();
            }
        }
    }

    /// dive into sub menu if applicable.
    /// Return: Some if entered deeper level
    ///         None if nothing happen
    pub fn push(&mut self) -> Option<()> {
        self.find_highlight()?.borrow_mut().highlight_first_child()
    }

    /// pop the current menu group. move one layer up
    pub fn pop(&mut self) {
        if let Some(item) = self.find_highlight() {
            item.borrow_mut().clear_highlight();
        }
    }

    /// clear all highlighted items. This is useful
    /// when the menu bar lose focus
    pub fn reset(&mut self) {
        self.root_item.clear_highlight();
    }

    /// client should drain events each frame, otherwise user action
    /// will feel laggy
    pub fn drain_events(&mut self) -> impl Iterator<Item = MenuEvent<T>> {
        std::mem::take(&mut self.events).into_iter()
    }

    /// return deepest highlight item's reference
    pub fn find_highlight(&self) -> Option<RefMenuItem<T>> {
        let (_parent, child) = self.find_highlight_parent_child();
        child
    }

    /// last but one layer in highlight.
    /// This will be the parent of the deepest highlight item
    pub fn find_highlight_parent(&mut self) -> Option<RefMenuItem<T>> {
        let (parent, _child) = self.find_highlight_parent_child();
        parent
    }

    /// Find deepest highlighted menu item and its parent
    ///
    /// Hidden root menu (None, None)
    /// Visible root menu, no children (None, Some(child))
    /// Drop down menu (Some(parent), Some(child))
    /// Impossible (Some(parent), None)
    fn find_highlight_parent_child(&self) -> (Option<RefMenuItem<T>>, Option<RefMenuItem<T>>) {
        if !self.root_item.borrow().is_highlight {
            return (None, None);
        }

        let mut parent = None;
        let mut child = Some(self.root_item.clone());

        loop {
            let next_child = child.as_ref().and_then(|c| c.highlight_child());
            if next_child.is_none() {
                break;
            }
            parent = child;
            child = next_child;
        }
        return (parent, child);
    }
    /* TODO
    /// Try to process an event. Return true if handled.
    pub fn on_event(&mut self, event: Event) -> Option<MenuAction> {
        match event {
            Event::Key(key_event) => keybinds().match_event(key_event),
            Event::MouseEvent(mouse_event) => {
                if menu.on_mouse_event(mouse_event) {
                    Some()
                }
            },
        }
    }
    */
    /// Try to handle a mouse event. If handled, return true
    pub fn on_mouse_event(&mut self, event: &MouseEvent) -> bool {
        let point = Position {
            x: event.column,
            y: event.row,
        };
        match event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                let root_item = self.root_item.clone();
                if let Some((parent, child_inx)) = root_item.find_menu_item(point) {
                    // Select item under cursor
                    let mut parent = parent.borrow_mut();
                    parent.clear_highlight();
                    parent.is_highlight = true;
                    parent.children[child_inx].borrow_mut().is_highlight = true;
                    drop(parent);
                    self.select();
                    return true;
                } else if self.is_active() {
                    // Click outside active menu will close it
                    self.reset();
                    return true;
                }
            }
            /* drag is kinda' complicated .. so not yet
            MouseEventKind::Moved | MouseEventKind::Drag(_) => {
                menu_item.find_highlight()
            }
            */
            _ => return false,
        }
        return false;
    }

    /// Handle a menu action, possibly updating the output queue
    /// See drain_events()
    pub fn handle_action(&mut self, action: MenuAction) {
        match action {
            // menu state
            MenuAction::Activate => self.activate(),
            MenuAction::Reset => self.reset(),

            // navigate
            MenuAction::Up => self.up(),
            MenuAction::Down => self.down(),
            MenuAction::Left => self.left(),
            MenuAction::Right => self.right(),

            // activate current menu item
            MenuAction::Select => self.select(),
        }
    }
}

/// Map events to actions. This is primarily useful for mapping
/// key-events, because mouse events contain a position that
/// usually affect which action they represent.
trait EventActionMap<Event, Action> {
    fn new(initial_map: Vec<(Event, Action)>) -> Self;
    fn add(&mut self, event: Event, action: Action);
    fn chain(&mut self, link: Self);
    fn map_event(&self, event: Event) -> Option<Action>;
}
type RefKeybind<Event, Action> = Rc<RefCell<Keybinds<Event, Action>>>;
#[derive(Default)]
pub struct Keybinds<Event, Action> {
    map: Vec<(Event, Action)>,
    extra: Vec<RefKeybind<Event, Action>>,
}

impl<Event, Action> EventActionMap<Event, Action> for RefKeybind<Event, Action>
  where
    Event: std::cmp::PartialEq + Copy,
    Action: Copy
  {
    fn new(initial_map: Vec<(Event, Action)>) -> Self {
        Rc::new(RefCell::new(Keybinds::<Event, Action> {
            map: initial_map,
            extra: vec![],
        }))
    }
    fn add(&mut self, event: Event, action: Action) {
        self.borrow_mut().map.push((event, action))
    }
    fn chain(&mut self, link: Self) {
        self.borrow_mut().extra.push(link)
    }
    fn map_event(&self, event: Event) -> Option<Action> {
        for (e, a) in self.borrow().map.iter() {
            if *e == event { return Some(*a) }
        }
        self.borrow().extra.iter()
            .find_map(|eam| eam.map_event(event))
    }

}

/// A MenuItem with this name will be rendered as a separator line
const MENU_LINE: &str = "";

/// MenuItem is the node in menu tree. If children is not
/// empty, then this item is the group item.
pub struct MenuItem<T> {
    name: Cow<'static, str>,
    pub data: Option<T>,
    /// Only enabled menu items can be selected
    pub enabled: bool,
    children: Vec<RefMenuItem<T>>,
    child_rect: Vec<Rect>,
    is_highlight: bool,
}

impl<T> MenuItem<T> {
    /// helper function to create a non group item.
    pub fn item(name: impl Into<Cow<'static, str>>, data: T) -> Self {
        Self {
            name: name.into(),
            data: Some(data),
            enabled: true,
            is_highlight: false,
            children: vec![],
            child_rect: vec![],
        }
    }

    /// helper function to create a group item.
    ///
    /// # Example
    ///
    /// ```
    /// use tui_menu::MenuItem;
    ///
    /// let item = MenuItem::<&'static str>::group("group", vec![
    ///     MenuItem::item("foo", "label_foo"),
    /// ]);
    ///
    /// assert!(item.is_group());
    ///
    /// ```
    pub fn group(name: impl Into<Cow<'static, str>>, children: Vec<Self>) -> Self {
        if children.get(0).filter(|c| c.name == MENU_LINE).is_some() {
            panic!("First menu item in a group must not be a line");
        }
        let child_count = children.len();
        Self {
            name: name.into(),
            data: None,
            enabled: true,
            is_highlight: false,
            children: children.into_iter().map(|c| RefMenuItem::new(c)).collect(),
            child_rect: vec![Rect::ZERO; child_count],
        }
    }

    /// helper function to create a separator line
    pub fn line() -> Self {
        Self {
            name: MENU_LINE.into(),
            data: None,
            enabled: true, // will use normal style
            is_highlight: false,
            children: vec![],
            child_rect: vec![],
        }
    }

    #[cfg(test)]
    fn with_highlight(mut self, highlight: bool) -> Self {
        self.is_highlight = highlight;
        self
    }

    /// whether this item is group
    pub fn is_group(&self) -> bool {
        !self.children.is_empty()
    }

    /// get current item's name
    fn name(&self) -> &str {
        &self.name
    }

    /// highlight first child
    fn highlight_first_child(&mut self) -> Option<()> {
        if !self.children.is_empty() {
            if let Some(it) = self.children.get_mut(0) {
                it.borrow_mut().is_highlight = true;
            }
            Some(())
        } else {
            None
        }
    }

    /// highlight prev item in this node
    fn highlight_prev(&mut self) {
        // if no child selected, then
        let Some(current_index) = self.highlight_child_index() else {
            self.highlight_first_child();
            return;
        };
        // Find index to highlight
        let mut cur = current_index;
        loop {
            let prev = cur;
            cur = cur.saturating_sub(1);
            if cur == prev {
                return; // No prev to highlight
            }
            if self.children[cur].borrow().name != MENU_LINE {
                break; // Found next to highlight
            }
        }
        let index_to_highlight = cur;
        // Highlight previous
        self.children[current_index].borrow_mut().clear_highlight();
        self.children[index_to_highlight].borrow_mut().is_highlight = true;
    }

    /// highlight prev item in this node
    fn highlight_next(&mut self) {
        // if no child selected, then
        let Some(current_index) = self.highlight_child_index() else {
            self.highlight_first_child();
            return;
        };
        // Find index to highlight
        let mut cur = current_index;
        loop {
            let prev = cur;
            cur = (cur + 1).min(self.children.len() - 1);
            if cur == prev {
                return; // No next to highlight
            }
            if self.children[cur].borrow().name != MENU_LINE {
                break; // Found next to highlight
            }
        }
        let index_to_highlight = cur;
        // Highlight next
        self.children[current_index].borrow_mut().clear_highlight();
        self.children[index_to_highlight].borrow_mut().is_highlight = true;
    }

    /// return highlighted child index
    fn highlight_child_index(&self) -> Option<usize> {
        for (idx, child) in self.children.iter().enumerate() {
            if child.borrow_mut().is_highlight {
                return Some(idx);
            }
        }

        None
    }

    /// if any child highlighted, then return its reference
    fn highlight_child(&self) -> Option<RefMenuItem<T>> {
        self.children
            .iter()
            .filter(|i| i.borrow().is_highlight)
            .nth(0)
            .and_then(|rmi| Some(rmi.clone()))
    }

    /// clear is_highlight flag recursively.
    fn clear_highlight(&mut self) {
        self.is_highlight = false;
        for child in self.children.iter_mut() {
            child.borrow_mut().clear_highlight();
        }
    }
}

/// StatefulWidget for displaying a menu.
///
/// The menu structure is stored in the state: [MenuState].
pub struct Menu<T> {
    /// style for default item style
    pub default_item_style: Style,
    /// style for highlighted item
    pub highlight_item_style: Style,
    /// style for disabled item
    pub disabled_item_style: Style,
    /// width for drop down panel
    drop_down_width: u16,
    /// style for drop down panel
    pub drop_down_style: Style,
    _priv: PhantomData<T>,
}

impl<T> Menu<T> {
    pub fn new() -> Self {
        Self {
            highlight_item_style: Style::default().bg(Color::LightBlue),
            default_item_style: Style::default().fg(Color::White),
            disabled_item_style: Style::default().fg(Color::DarkGray),
            drop_down_width: 20,
            drop_down_style: Style::default().bg(Color::Gray),
            _priv: Default::default(),
        }
    }

    /// update with highlight style
    pub fn default_style(mut self, style: Style) -> Self {
        self.default_item_style = style;
        self
    }

    /// update with highlight style
    pub fn highlight_style(mut self, style: Style) -> Self {
        self.highlight_item_style = style;
        self
    }
    /// update disabled style
    pub fn disabled_style(mut self, style: Style) -> Self {
        self.highlight_item_style = style;
        self
    }

    /// update drop_down_width
    pub fn dropdown_width(mut self, width: u16) -> Self {
        self.drop_down_width = width;
        self
    }

    /// update drop_down fill style
    pub fn dropdown_style(mut self, style: Style) -> Self {
        self.drop_down_style = style;
        self
    }

    /// render an item group in drop down
    /* Each menu item in the group is rendered like this
    .|.NameString.|.
      ^^^^^^^^^^^^ ------ this area will be highlighted
    */
    fn render_dropdown(
        &self,
        x: u16,
        y: u16,
        group: &mut MenuItem<T>,
        buf: &mut ratatui::buffer::Buffer,
        dropdown_count_to_go: u16, // including current, it is not drawn yet
    ) {
        // Compute width of all menu items
        let child_max_width = group
            .children
            .iter()
            .map(|ref_menu_item| ref_menu_item.borrow())
            .map(|menu_item| Span::from(menu_item.name.clone()).width())
            .max()
            .unwrap_or(0) as u16;
        let min_drop_down_width: u16 = (child_max_width + 6) as u16;
        let min_drop_down_height: u16 = (group.children.len() + 2) as u16;

        // prevent calculation issue if canvas is narrow
        let drop_down_width = self.drop_down_width.min(buf.area.width);

        // calculate the maximum x, leaving enough space for deeper items
        // drawing area:
        // |  a |  b   |            c                |        d       |
        // | .. |  me  |  child_1  |  child_of_child |  nothing here  |
        // x_max is the x when d is 0
        let b_plus_c = dropdown_count_to_go * drop_down_width;
        let x_max = buf.area().right().saturating_sub(b_plus_c);

        let x = x.min(x_max);

        let area = Rect::new(x, y, min_drop_down_width, min_drop_down_height);

        // clamp to ensure we draw in areas
        let area = area.clamp(*buf.area());

        Clear.render(area, buf);

        buf.set_style(area, self.default_item_style);

        // Render menu border
        use ratatui::prelude::Margin;
        use ratatui::widgets::Block;
        use ratatui::widgets::Borders;
        let border = Block::default()
            .borders(Borders::ALL)
            .style(self.default_item_style);
        border.render(
            area.inner(Margin {
                vertical: 0,
                horizontal: 1,
            }),
            buf,
        );

        // Render menu items
        let mut active_group: Option<_> = None;
        let mut child_rect = vec![];
        for (idx, child) in group.children.iter().enumerate() {
            let item = child.borrow();
            let item_x = x + 2;
            let item_y = y + 1 + idx as u16;
            let is_active = item.is_highlight;

            let item_name = item.name();

            if item_name == MENU_LINE {
                let line = Block::default()
                    .borders(Borders::TOP)
                    .style(self.default_item_style);
                let rect = Rect {
                    x: item_x,
                    y: item_y,
                    width: area.width - 4,
                    height: 1,
                };
                line.render(rect, buf);
                child_rect.push(rect);
                continue; // TODO rearrange code to avoid this jump
            }

            // make style apply to whole line by make name whole line
            let mut item_name =
                format!(" {: <width$} ", item_name, width = child_max_width as usize);

            if !item.children.is_empty() {
                item_name.pop();
                item_name.push('>');
            }

            let rect = Rect {
                x: item_x,
                y: item_y,
                width: child_max_width + 2,
                height: 1,
            };

            let mut item_style = if item.enabled {
                self.default_item_style
            } else {
                self.disabled_item_style
            };
            if is_active {
                item_style = item_style.set_style(self.highlight_item_style);
            }
            buf.set_span(
                rect.x,
                rect.y,
                &Span::styled(
                    item_name,
                    item_style,
                ),
                rect.width,
            );

            child_rect.push(rect);

            if is_active && !item.children.is_empty() {
                active_group = Some((item_x + child_max_width, item_y, child.clone()));
            }
        }
        group.child_rect = child_rect;

        // draw at the end to ensure its content above all items in current level
        if let Some((x, y, group)) = active_group {
            self.render_dropdown(
                x,
                y,
                &mut *group.borrow_mut(),
                buf,
                dropdown_count_to_go - 1,
            );
        }
    }
}

impl<T> Default for Menu<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> StatefulWidget for &Menu<T> {
    type State = MenuState<T>;

    fn render(self, area: Rect, buf: &mut ratatui::buffer::Buffer, state: &mut Self::State) {
        let area = area.clamp(*buf.area());

        let mut spans = vec![];
        let mut rects = vec![];
        let mut x_pos = area.x;
        let y_pos = area.y;

        let dropdown_count = state.dropdown_count();

        // Skip top left char
        spans.push(Span::raw(" ").style(self.default_item_style));

        for child in state.root_item.borrow().children.iter() {
            let item = &mut *child.borrow_mut();
            let is_highlight = item.is_highlight;
            let has_children = !item.children.is_empty();

            // Render horizontal menu at top
            let item_style = if is_highlight {
                self.highlight_item_style
            } else {
                self.default_item_style
            };
            let span = Span::styled(format!(" {} ", item.name()), item_style);
            let rect = Rect {
                x: x_pos,
                y: y_pos,
                width: span.width() as u16,
                height: 1,
            };
            spans.push(span);
            rects.push(rect);


            x_pos += rect.width;

            // Render vertical menu below top menu item
            if has_children && is_highlight {
                self.render_dropdown(rect.x, rect.bottom(), item, buf, dropdown_count);
            }
        }
        state.root_item.borrow_mut().child_rect = rects;
        buf.set_line(area.x, area.y, &Line::from(spans), area.width);
    }
}

#[cfg(test)]
mod tests {
    use crate::MenuState;

    type MenuItem = super::MenuItem<i32>;

    #[test]
    fn test_active_depth() {
        {
            let menu_state = MenuState::new(vec![MenuItem::item("item1", 0)]);
            assert_eq!(menu_state.active_depth(), 0);
        }

        {
            let menu_state = MenuState::new(vec![MenuItem::item("item1", 0).with_highlight(true)]);
            assert_eq!(menu_state.active_depth(), 1);
        }

        {
            let menu_state = MenuState::new(vec![MenuItem::group("layer1", vec![])]);
            assert_eq!(menu_state.active_depth(), 0);
        }

        {
            let menu_state =
                MenuState::new(vec![MenuItem::group("layer1", vec![]).with_highlight(true)]);
            assert_eq!(menu_state.active_depth(), 1);
        }

        {
            let menu_state = MenuState::new(vec![MenuItem::group(
                "layer_1",
                vec![MenuItem::item("item_layer_2", 0)],
            )
            .with_highlight(true)]);
            assert_eq!(menu_state.active_depth(), 1);
        }

        {
            let menu_state = MenuState::new(vec![MenuItem::group(
                "layer_1",
                vec![MenuItem::item("item_layer_2", 0).with_highlight(true)],
            )
            .with_highlight(true)]);
            assert_eq!(menu_state.active_depth(), 2);
        }
    }

    #[test]
    fn test_dropdown_count() {
        {
            // only item in menu bar
            let menu_state = MenuState::new(vec![MenuItem::item("item1", 0)]);
            assert_eq!(menu_state.dropdown_count(), 0);
        }

        {
            // group in menu bar,
            let menu_state = MenuState::new(vec![MenuItem::group(
                "menu bar",
                vec![MenuItem::item("item layer 1", 0)],
            )
            .with_highlight(true)]);
            assert_eq!(menu_state.dropdown_count(), 1);
        }

        {
            // group in menu bar,
            let menu_state = MenuState::new(vec![MenuItem::group(
                "menu bar 1",
                vec![
                    MenuItem::group("dropdown 1", vec![MenuItem::item("item layer 2", 0)])
                        .with_highlight(true),
                    MenuItem::item("item layer 1", 0),
                ],
            )
            .with_highlight(true)]);
            assert_eq!(menu_state.dropdown_count(), 2);
        }

        {
            // *menu bar 1
            // *dropdown 1   >  item layer 2
            // item layer 1    group layer 2 >
            let menu_state = MenuState::new(vec![MenuItem::group(
                "menu bar 1",
                vec![
                    MenuItem::group(
                        "dropdown 1",
                        vec![
                            MenuItem::item("item layer 2", 0),
                            MenuItem::group(
                                "group layer 2",
                                vec![MenuItem::item("item layer 3", 0)],
                            ),
                        ],
                    )
                    .with_highlight(true),
                    MenuItem::item("item layer 1", 0),
                ],
            )
            .with_highlight(true)]);
            assert_eq!(menu_state.dropdown_count(), 2);
        }

        {
            // *menu bar 1
            // *dropdown 1   >  *item layer 2
            // item layer 1    group layer 2 > item layer 3
            let menu_state = MenuState::new(vec![MenuItem::group(
                "menu bar 1",
                vec![
                    MenuItem::group(
                        "dropdown 1",
                        vec![
                            MenuItem::item("item layer 2", 0).with_highlight(true),
                            MenuItem::group(
                                "group layer 2",
                                vec![MenuItem::item("item layer 3", 0)],
                            ),
                        ],
                    )
                    .with_highlight(true),
                    MenuItem::item("item layer 1", 0),
                ],
            )
            .with_highlight(true)]);
            assert_eq!(menu_state.dropdown_count(), 3);
        }
    }
}
