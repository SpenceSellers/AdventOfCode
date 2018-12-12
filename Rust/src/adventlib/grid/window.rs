use super::*;

pub struct GridWindow<G> {
    grid: G,
    region: RectangleBounds
}

impl<G: GridView> GridView for GridWindow<G> {
    type Item = G::Item;
    fn get_cell(&self, pos: Point) -> Self::Item {
        let adjusted = pos.subtract(self.region.lesser());
        self.grid.get_cell(pos)
    }
}

impl<G> DefinedSizeGrid for GridWindow<G> {
    fn width(&self) -> usize { self.region.width() as usize }
    fn height(&self) -> usize { self.region.height() as usize }
}