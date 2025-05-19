// using hash maps to count the birds since hash maps are unordered collections which 
// is perfect because we don't care about the order of the birds but only the count
use std::collections::HashMap;

// This function returns how many times the most frequently seen bird appeared
pub fn find_most_frequent_bird_count(birds: &[&str]) -> usize {
    let mut counts = HashMap::new(); // Create a map to count bird sightings

    // Go through all bird IDs
    for &bird in birds {
        // Add or update the count for this bird
        let counter = counts.entry(bird).or_insert(0);
        *counter += 1;
    }

    // Find the highest count (or return 0 if there were no birds)
    let mut max_count = 0;
    for count in counts.values() {
        if *count > max_count {
            max_count = *count;
        }
    }

    max_count
}

// This function returns any one of the birds that was seen the most
pub fn find_most_frequent_bird_no_order(birds: &[&str]) -> Option<String> {
    let mut counts = HashMap::new();

    // Count each bird
    for &bird in birds {
        let counter = counts.entry(bird).or_insert(0);
        *counter += 1;
    }

    let mut most_seen_bird = None;
    let mut max_count = 0;

    // Go through all bird counts to find one with the highest count
    for (bird, &count) in counts.iter() {
        if count > max_count {
            max_count = count;
            most_seen_bird = Some(bird.to_string());
        }
    }

    most_seen_bird
}

// This function returns the bird that was seen the most,
// but if there's a tie, it returns the one that was seen first
pub fn find_most_frequent_bird(birds: &[&str]) -> Option<String> {
    let mut counts = HashMap::new();
    let mut order = Vec::new(); // Stores the order birds were first seen

    // Count birds and remember the order they first appeared
    for &bird in birds {
        let counter = counts.entry(bird).or_insert(0);
        if *counter == 0 {
            order.push(bird); // Only store it once
        }
        *counter += 1;
    }

    // Find the highest count
    let mut max_count = 0;
    for count in counts.values() {
        if *count > max_count {
            max_count = *count;
        }
    }

    // Go through the first-seen order to find the first bird with max count
    for &bird in &order {
        if counts[bird] == max_count {
            return Some(bird.to_string());
        }
    }

    None
}


// test module
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_most_frequent_bird_count() {
        let birds = vec!["AA", "BB", "AA", "BB", "CC", "BB"];
        assert_eq!(find_most_frequent_bird_count(&birds), 3);
    }

    #[test]
    fn test_find_most_frequent_bird_no_order() {
        let birds = vec!["AA", "BB", "AA", "BB", "CC", "BB"];
        let result = find_most_frequent_bird_no_order(&birds);
        assert!(result == Some("BB".to_string()) || result == Some("AA".to_string()));
    }

    #[test]
    fn test_find_most_frequent_bird() {
        let birds = vec!["AA", "BB", "AA", "BB", "CC", "BB"];
        assert_eq!(find_most_frequent_bird(&birds), Some("BB".to_string()));
    }
}
