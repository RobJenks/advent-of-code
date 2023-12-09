use std::cmp::Ordering;
use itertools::Itertools;
use crate::day7::cards::HandTypeName::{FiveOfAKind, FourOfAKind, FullHouse, HighCard, OnePair, ThreeOfAKind, TwoPair, Unknown};

static CARDS : [Card; 13] = [
    Card { name: '2', value: 2 },
    Card { name: '3', value: 3 },
    Card { name: '4', value: 4 },
    Card { name: '5', value: 5 },
    Card { name: '6', value: 6 },
    Card { name: '7', value: 7 },
    Card { name: '8', value: 8 },
    Card { name: '9', value: 9 },
    Card { name: 'T', value: 10 },
    Card { name: 'J', value: 11 },
    Card { name: 'Q', value: 12 },
    Card { name: 'K', value: 13 },
    Card { name: 'A', value: 14 }
];

pub const HAND_FIVE_OF_A_KIND: HandType = HandType { name: FiveOfAKind, power: 7 };
pub const HAND_FOUR_OF_A_KIND: HandType = HandType { name: FourOfAKind, power: 6 };
pub const HAND_FULL_HOUSE: HandType = HandType { name: FullHouse, power: 5 };
pub const HAND_THREE_OF_A_KIND: HandType = HandType { name: ThreeOfAKind, power: 4 };
pub const HAND_TWO_PAIR: HandType = HandType { name: TwoPair, power: 3 };
pub const HAND_ONE_PAIR: HandType = HandType { name: OnePair, power: 2 };
pub const HAND_HIGH_CARD: HandType = HandType { name: HighCard, power: 1 };
pub const HAND_UNKNOWN: HandType = HandType { name: Unknown, power: 0 };

#[derive(Clone, Copy, Debug)]
pub struct Card {
    name: char,
    value: u32
}

#[derive(Debug, Clone)]
pub struct Hand {
    pub cards: [Card; 5],
    pub hand_type: HandType
}

#[derive(Debug, Copy, Clone)]
pub enum HandTypeName {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
    Unknown
}

#[derive(Debug, Clone)]
pub struct HandType {
    #[allow(unused)]
    name: HandTypeName,
    power: u32,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Round {
    pub hand: Hand,
    pub bid: usize
}

impl Card {
    pub fn get_name(&self) -> char { self.name }
}

impl PartialEq for Card {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl Eq for Card { /* Is equivalence relation */ }

impl PartialOrd for HandType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for HandType {
    fn cmp(&self, other: &Self) -> Ordering {
        self.power.cmp(&other.power)
    }
}
impl PartialEq for HandType {
    fn eq(&self, other: &Self) -> bool {
        self.power == other.power
    }
}
impl Eq for HandType { /* Is equivalence relation */ }


impl Hand {
    pub fn new(card_names: Vec<char>) -> Self {
        Self {
            cards: {
                let c = card_names.iter()
                    .map(|&name| CARDS.iter().find(|&c| c.name == name)
                        .cloned()
                        .unwrap_or_else(|| panic!("Unknown card type '{}'", name)))
                    .collect_vec();
                [c[0], c[1], c[2], c[3], c[4]]
            },
            hand_type: HAND_UNKNOWN
        }
    }

    pub fn get_card_wise_rank(&self, other: &Hand) -> Ordering {
        self.cards.iter().zip(other.cards)
            .map(|(this, cmp)| this.value.cmp(&cmp.value))
            .filter(|&cmp| cmp != Ordering::Equal)
            .next()
            .unwrap_or_else(|| panic!("Could not compare card-wise hand rank"))
    }

    pub fn get_hand_type(hand: &Hand) -> HandType {
        let counts = hand.cards.iter().map(Card::get_name).counts();

        if counts.len() == 1 {
            return HAND_FIVE_OF_A_KIND;
        }

        if counts.len() == 2 {
            let &num = counts.iter().next().unwrap().1;
            if num == 1 || num == 4 { return HAND_FOUR_OF_A_KIND; }
            if num == 2 || num == 3 { return HAND_FULL_HOUSE; }
        }

        if counts.len() == 3 {
            return if counts.iter().find(|(_, &n)| n == 3).is_some() { HAND_THREE_OF_A_KIND }
            else { HAND_TWO_PAIR }
        }

        if counts.len() == 4 {
            return HAND_ONE_PAIR;
        }

        assert_eq!(counts.len(), 5);
        HAND_HIGH_CARD
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        let by_type = self.hand_type.cmp(&other.hand_type);
        if by_type != Ordering::Equal {
            by_type
        }
        else {
            self.get_card_wise_rank(other)
        }
    }
}
impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.cards == other.cards
    }
}
impl Eq for Hand { /* Is equivalence relation */ }

impl Round {
    pub fn new(hand: Hand, bid: usize) -> Self {
        Self { hand, bid }
    }
}
