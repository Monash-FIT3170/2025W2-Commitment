import React, { useEffect, useState } from 'react';
import { Button } from '@ui/components/ui/button'; 
import { Meteor } from 'meteor/meteor';
import { Bookmark } from 'lucide-react';

type BookmarkRepoButtonProps = {
    url: string;
    title: string;
};

const BookmarkRepoButton: React.FC<BookmarkRepoButtonProps> = ({ url, title }) => {
    const [isBookmarked, setIsBookmarked] = useState(false);
    const [loading, setLoading] = useState(true);

    // Check if already bookmarked
    useEffect(() => {
        Meteor.call('links.isBookmarked', url, (err: any, result: boolean) => {
            if (!err) {
                setIsBookmarked(result);
            }
            setLoading(false);
        });
    }, [url]);

    const handleClick = () => {
        if (!url || !title) {
            alert('Missing URL or title');
            return;
        }

        if (isBookmarked) {
            const confirmUnbookmark = confirm('Do you want to remove this bookmark?');
            if (confirmUnbookmark) {
                Meteor.call('links.remove', url, (err: any) => {
                    if (err) {
                        alert(`Failed to remove bookmark: ${err.reason}`);
                    } else {
                        alert('Bookmark removed!');
                        setIsBookmarked(false);
                    }
                });
            }
        } else {
            Meteor.call('links.insert', title, url, (err: any) => {
                if (err) {
                    alert(`Failed to save repo: ${err.reason}`);
                } else {
                    alert('Repository saved!');
                    setIsBookmarked(true);
                }
            });
        }
    };

    return (
        <Button
            variant={isBookmarked ? 'secondary' : 'default'}
            size="default"
            onClick={handleClick}
            disabled={loading}
            className="flex items-center gap-2"
        >
            <Bookmark
                className="w-4 h-4"
                fill={isBookmarked ? 'currentColor' : 'none'}
                stroke="currentColor"
            />
            {isBookmarked ? 'Bookmarked' : 'Bookmark'}
        </Button>
    );
};

export default BookmarkRepoButton;
