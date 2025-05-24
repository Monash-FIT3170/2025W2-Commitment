// /imports/ui/widgets/BookmarkButtonTest.tsx
import React from 'react';
import BookmarkRepoButton from '../imports/ui/components/ui/widgets/BookmarkButton';

/**
 * BookmarkButtonTest is a manual visual test component that demonstrates how
 * the BookmarkRepoButton behaves under various input conditions.
 *
 * Test cases included:
 * 1. Valid URL and title: Should render a functional button.
 * 2. Invalid URL: Should show how the component handles malformed URLs.
 * 3. Missing URL and title: Should test the component's behavior with empty input.
 *
 * To use: 
 * 1. Render this component in the main App.tsx
 * 2. Ensure the Toaster component is also setup in the App
 * 3. Run meteor and click the three buttons to see the behavior
 */
const BookmarkButtonTest = () => {
    const mockUrl1 = 'https://example.com/repo';
    const mockTitle1 = 'Example Repo: Valid';
    const mockUrl2 = 'htts://example.com/repo';

    return (
    <div className="p-4">
        <h2 className="text-xl font-semibold mb-4">Bookmark Button Test</h2>
        Valid URL and Title
        <BookmarkRepoButton url={mockUrl1} title={mockTitle1} />
        Invalid URL
        <BookmarkRepoButton url={mockUrl2} title={mockTitle1} />
        Missing URL and Title
        <BookmarkRepoButton url="" title="" />
    </div>
    );
};

export default BookmarkButtonTest;