// /imports/ui/widgets/BookmarkButtonTest.tsx
import React from "react";
import BookmarkButton from "../imports/ui/components/dashboard/BookmarkButton";

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
function BookmarkButtonTest() {
  const mockUrl1 = "https://example.com/repo";
  const mockTitle1 = "Example Repo: Valid";
  const mockUrl2 = "htts://example.com/repo";
  const mockUserID = "testUser123";

  return (
    <div className="p-4">
      <h2 className="text-xl font-semibold mb-4">Bookmark Button Test</h2>
      Valid URL and Title
      <BookmarkButton url={mockUrl1} title={mockTitle1} currentUserID={mockUserID} />
      Invalid URL
      <BookmarkButton url={mockUrl2} title={mockTitle1} currentUserID={mockUserID} />
      Missing URL and Title
      <BookmarkButton url="" title="" currentUserID={mockUserID} />
    </div>
  );
}

export default BookmarkButtonTest;
