#[derive(Debug)]
pub struct Data {
    pub seeds : Vec<usize>,

    pub seed_to_soil: DataMap,
    pub soil_to_fert: DataMap,
    pub fert_to_water: DataMap,
    pub water_to_light: DataMap,
    pub light_to_temp: DataMap,
    pub temp_to_humid: DataMap,
    pub humid_to_location: DataMap
}

#[derive(Debug)]
pub struct DataMap {
    pub data: Vec<Mapping>
}

#[derive(Debug)]
#[allow(unused)]
pub struct Mapping {
    source_start: usize,
    source_end: usize,
    dest_start: usize,
    dest_end: usize,
    size: usize
}

impl Data {
    pub fn new(seeds: Vec<usize>) -> Self {
        Self {
            seeds,
            seed_to_soil: DataMap::new(),
            soil_to_fert: DataMap::new(),
            fert_to_water: DataMap::new(),
            water_to_light: DataMap::new(),
            light_to_temp: DataMap::new(),
            temp_to_humid: DataMap::new(),
            humid_to_location: DataMap::new()
        }
    }

    pub fn get_data_collection_mut(&mut self, ix: usize) -> &mut DataMap {
        match ix {
            0 => &mut self.seed_to_soil,
            1 => &mut self.soil_to_fert,
            2 => &mut self.fert_to_water,
            3 => &mut self.water_to_light,
            4 => &mut self.light_to_temp,
            5 => &mut self.temp_to_humid,
            6 => &mut self.humid_to_location,
            _ => panic!("Invalid collection index {}", ix)
        }
    }
}

impl DataMap {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn add(&mut self, mapping: Mapping) {
        self.data.push(mapping);
    }

    pub fn map(&self, value: usize) -> usize {
        self.data.iter()
            .filter(|&map| value >= map.source_start && value <= map.source_end)
            .next()

            .map(|map| map.dest_start + (value - map.source_start))
            .unwrap_or(value)
    }
}

impl Mapping {
    pub fn new(source_start: usize, dest_start: usize, size: usize) -> Self {
        Self { source_start, dest_start, size, source_end: source_start + size - 1, dest_end: dest_start + size - 1 }
    }
}
