// Ensure the script runs after the entire HTML document is loaded.
document.addEventListener('DOMContentLoaded', () => {
    // Get references to key HTML elements.
    const announcementsList = document.getElementById('announcements-list');
    const searchInput = document.getElementById('searchInput');
    const clearSearchButton = document.getElementById('clearSearch');
    const scrollToTopButton = document.getElementById('scrollToTop');

    // Collect all announcement elements that were rendered on the page.
    const allAnnouncementElements = Array.from(announcementsList.children).filter(el => el.classList.contains('announcement'));

    // Add "Read More" functionality to each announcement.
    allAnnouncementElements.forEach(announcementDiv => {
        const contentDiv = announcementDiv.querySelector('.content');
        if (contentDiv) {
            // Use a small timeout to ensure content has rendered before checking for overflow.
            setTimeout(() => {
                // Check if the content overflows its container.
                if (contentDiv.scrollHeight > contentDiv.clientHeight + 5) {
                    const readMoreButton = document.createElement('button');
                    readMoreButton.classList.add('read-more-button');
                    readMoreButton.textContent = 'Read More';

                    // Toggle content expansion and button text on click.
                    readMoreButton.addEventListener('click', () => {
                        contentDiv.classList.toggle('expanded');
                        readMoreButton.textContent = contentDiv.classList.contains('expanded') ? 'Show Less' : 'Read More';
                    });
                    announcementDiv.appendChild(readMoreButton);
                }
            }, 100); // Short delay.
        }
    });

    // Implement search/filter logic for announcements.
    searchInput.addEventListener('keyup', (event) => {
        const searchTerm = event.target.value.toLowerCase();

        // Iterate through announcements and show/hide based on search term.
        allAnnouncementElements.forEach(announcementDiv => {
            const title = announcementDiv.querySelector('h3').textContent.toLowerCase();
            const content = announcementDiv.querySelector('.content').textContent.toLowerCase();

            if (title.includes(searchTerm) || content.includes(searchTerm)) {
                announcementDiv.style.display = 'block'; // Show if matches.
            } else {
                announcementDiv.style.display = 'none'; // Hide if no match.
            }
        });

        // Display "No results" message if no announcements are visible.
        const visibleAnnouncements = allAnnouncementElements.filter(el => el.style.display !== 'none');
        let noResultsMessage = document.querySelector('.no-results-message');

        if (visibleAnnouncements.length === 0 && searchTerm !== '') {
            if (!noResultsMessage) { // Create message if it doesn't exist.
                noResultsMessage = document.createElement('p');
                noResultsMessage.classList.add('no-results-message');
                announcementsList.appendChild(noResultsMessage);
            }
            noResultsMessage.textContent = 'No announcements found matching your search.';
        } else if (noResultsMessage && noResultsMessage.parentNode) {
            noResultsMessage.parentNode.removeChild(noResultsMessage); // Remove message if results appear.
        }
    });

    // Clear search input and show all announcements.
    clearSearchButton.addEventListener('click', () => {
        searchInput.value = '';
        allAnnouncementElements.forEach(announcementDiv => {
            announcementDiv.style.display = 'block'; // Show all announcements.
        });
        const noResultsMessage = document.querySelector('.no-results-message');
        if (noResultsMessage) {
            noResultsMessage.parentNode.removeChild(noResultsMessage); // Remove "No results" message.
        }
    });

    // Show/hide "Scroll to Top" button based on scroll position.
    window.addEventListener('scroll', () => {
        if (window.scrollY > 200) {
            scrollToTopButton.style.display = 'flex'; // Show button.
        } else {
            scrollToTopButton.style.display = 'none'; // Hide button.
        }
    });

    // Scroll to the top of the page when the button is clicked.
    scrollToTopButton.addEventListener('click', () => {
        window.scrollTo({
            top: 0,
            behavior: 'smooth' // Smooth scrolling effect.
        });
    });
});