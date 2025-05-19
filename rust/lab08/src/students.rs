// This function takes student data as a string and returns 
// how many students have the highest age.
pub fn count_oldest_students(data: &str) -> usize {
    let mut max_age = 0;    // This will store the highest age found
    let mut count = 0;      // This will count how many students have that age

    // Go through each line in the input string
    for line in data.lines() {
        let line = line.trim(); // Remove spaces at the start and end

        if line.is_empty() {
            continue; // Skip empty lines
        }

        // Split the line into words: first name, last name, and age
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts.len() != 3 {
            continue; // Skip lines that don't have exactly 3 parts
        }

        // Try to convert the age part (the 3rd item) into a number
        let age_result = parts[2].parse::<u32>();

        // Only continue if the age was a valid number
        if let Ok(age) = age_result {
            if age > max_age {
                max_age = age;   // Found a new highest age
                count = 1;       // Start counting from 1
            } else if age == max_age {
                count += 1;      // Another student with the same highest age
            }
        }
    }

    // Return how many students have the highest age
    count
}

// Unit tests for the count_oldest_students function
#[cfg(test)]
mod tests {
    use super::*; // Import the function to be tested

    #[test]
    fn test_count_oldest_students() {
        let data = "
Alice Cooper 25
Alice Boa 23
Bob Marley 23
Alice Chains 25
Charlie Brown 21
Charlie Chaplin 25
Eve Wonder 24
Sandra White 21

";
        assert_eq!(count_oldest_students(data), 3);
    }
}
