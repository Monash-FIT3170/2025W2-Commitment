/**
 * Utility functions for fetching and processing API README content
 */

import { Meteor } from 'meteor/meteor';

/**
 * Fetches the API README file content using Meteor method
 */
export const fetchAPIReadme = async (): Promise<string> => {
  return new Promise((resolve, reject) => {
    console.log('Calling Meteor method: api.getReadme');
    Meteor.call('api.getReadme', (error: any, result: string) => {
      if (error) {
        console.error('Meteor method error:', error);
        console.error('Error details:', error.reason, error.details);
        reject(error);
      } else {
        console.log('Meteor method success, result length:', result?.length);
        resolve(result);
      }
    });
  });
};

/**
 * Converts basic Markdown to HTML
 */
export const markdownToHtml = (markdown: string): string => {
  return markdown
    // Headers
    .replace(/^### (.*$)/gim, '<h3 class="text-lg font-semibold text-git-text-primary mb-2">$1</h3>')
    .replace(/^## (.*$)/gim, '<h2 class="text-xl font-semibold text-git-text-primary mb-3">$1</h2>')
    .replace(/^# (.*$)/gim, '<h1 class="text-2xl font-bold text-git-text-primary mb-4">$1</h1>')
    
    // Code blocks
    .replace(/```([\s\S]*?)```/g, '<pre class="bg-git-bg-secondary border border-git-stroke-secondary rounded-lg p-4 my-4 overflow-x-auto"><code class="text-git-text-secondary text-sm">$1</code></pre>')
    
    // Inline code
    .replace(/`([^`]+)`/g, '<code class="bg-git-bg-primary px-1 rounded text-git-text-primary text-sm">$1</code>')
    
    // Bold text
    .replace(/\*\*(.*?)\*\*/g, '<strong class="font-semibold text-git-text-primary">$1</strong>')
    
    // Links
    .replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2" target="_blank" rel="noopener noreferrer" class="text-git-accent-primary hover:text-git-accent-primary-hover underline">$1</a>')
    
    // Lists
    .replace(/^\* (.*$)/gim, '<li class="text-git-text-secondary mb-1">$1</li>')
    .replace(/(<li.*<\/li>)/gim, '<ul class="list-disc list-inside text-git-text-secondary space-y-1 my-2">$1</ul>')
    
    // Paragraphs (double newlines)
    .replace(/\n\n/g, '</p><p class="text-git-text-secondary mb-3">')
    
    // Single line breaks
    .replace(/\n/g, '<br>')
    
    // Wrap in paragraph tags
    .replace(/^/, '<p class="text-git-text-secondary mb-3">')
    .replace(/$/, '</p>');
};

/**
 * Gets the API README content as HTML
 */
export const getAPIReadmeAsHtml = async (): Promise<string> => {
  try {
    const markdown = await fetchAPIReadme();
    return markdownToHtml(markdown);
  } catch (error) {
    console.error('Error processing API README:', error);
    return '<p class="text-git-text-secondary">Unable to load API documentation. Please try again later.</p>';
  }
};
