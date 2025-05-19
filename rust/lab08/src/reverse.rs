// This function takes a string slice and returns the reversed string wihtout using the built-in .rev() method.
pub fn reverse(input: &str) -> String {
    // Convert the string into a vector of characters and collect them
    let chars: Vec<char> = input.chars().collect();

    // Create an empty String with enough space to hold the result
    let mut result = String::with_capacity(input.len());

    // Start from the end of the character vector and work backwards
    let mut i = chars.len();
    while i > 0 {
        i -= 1;
        result.push(chars[i]); // Add each character to the result
    }

    result
}

// Unit tests for the reverse function
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reverse_hello() {
        let input = "hello";
        let expected = "olleh";
        let result = reverse(input);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_reverse_rust() {
        let input = "rust";
        let expected = "tsur";
        let result = reverse(input);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_reverse_empty_string() {
        let input = "";
        let expected = "";
        let result = reverse(input);
        assert_eq!(result, expected);
    }
}
