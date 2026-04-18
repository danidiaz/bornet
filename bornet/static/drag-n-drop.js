
document.addEventListener('DOMContentLoaded', function() {
    const body = document.body;
    
    // Single dragover listener on body
    body.addEventListener('dragover', function(e) {
        // Check if we're over a table cell
        if (e.target.tagName === 'TD') {
            e.preventDefault(); // Make it a valid drop target
        }
    });
    
    /*
    body.addEventListener('dragenter', function(e) {
        if (e.target.tagName === 'TD') {
            e.preventDefault();
            e.target.classList.add('drag-over');
        }
    });
    
    body.addEventListener('dragleave', function(e) {
        if (e.target.tagName === 'TD') {
            e.target.classList.remove('drag-over');
        }
    });
    */
    
    // Handle drops
    body.addEventListener('drop', function(e) {
        if (e.target.tagName === 'TD') {
            e.preventDefault();
            // e.target.classList.remove('drag-over');
            
            // Your drop logic here
            console.log('Dropped on cell:', e.target);
        }
    });
});