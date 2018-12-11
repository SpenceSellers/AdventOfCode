use super::*;

pub struct GridWindow<G> {
    grid: G,
    region: RectangleBounds
}

impl<G: GridView> GridView for GridWindow<G> {
    type Item = G::Item;
    fn get_cell(&self, pos: Point) -> Self::Item {
        self.grid.get_cell(pos)
    }
}