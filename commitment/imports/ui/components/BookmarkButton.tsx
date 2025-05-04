import React from 'react';
import { Button } from '@ui/components/ui/button'; // Adjust to your actual path
import { Meteor } from 'meteor/meteor';

type BookmarkRepoButtonProps = {
    url: string;
    title: string;
};

const BookmarkRepoButton: React.FC<BookmarkRepoButtonProps> = ({ url, title }) => {
    const handleClick = () => {
        if (!url || !title) {
            alert('Missing URL or title');
            return;
        }

        Meteor.call('links.insert', title, url, (err: any) => {
            if (err) {
                alert(`Failed to save repo: ${err.reason}`);
            } else {
                alert('Repository saved!');
            }
        });
    };

    return (
        <Button variant="default" size="default" onClick={handleClick}>
            //TODO: Add icon and styling here 
        </Button>
    );
};

export default BookmarkRepoButton;