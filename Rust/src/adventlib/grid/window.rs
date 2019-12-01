use super::*;

pub struct GridWindow<G> {
    pub grid: G,
    pub region: RectangleBounds
}

impl<G: GridView> GridView for GridWindow<G> {
    type Item = G::Item;
    fn get_cell(self, pos: Point) -> Self::Item {
        // Todo block too large
        let adjusted = pos.subtract_point(self.region.lesser());
        self.grid.get_cell(adjusted)
    }
}

impl<G: GridView> DefinedSizeGrid for GridWindow<G> {
    fn width(&self) -> usize { self.region.width() as usize }
    fn height(&self) -> usize { self.region.height() as usize }
}