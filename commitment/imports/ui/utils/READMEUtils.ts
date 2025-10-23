/**
 * Utility functions for fetching and processing API README content
 */

import { meteorCallAsync, extractErrMsg } from "/imports/api/meteor_interface";

// Import Prism.js for syntax highlighting
import Prism from 'prismjs';
import 'prismjs/components/prism-javascript';
import 'prismjs/components/prism-typescript';
import 'prismjs/components/prism-bash';
import 'prismjs/components/prism-json';

/**
 * Fetches the API README file content using Meteor method
 */
export const fetchAPIReadme = async (): Promise<string> =>
  meteorCallAsync<string>("api.getReadme")().catch(extractErrMsg);

/**
 * Highlights code using Prism.js
 */
const highlightCode = (code: string, language: string = 'javascript'): string => {
  try {
    // Get the language grammar, fallback to javascript
    const grammar = Prism.languages[language] || Prism.languages.javascript;
    
    // Highlight the code
    const highlighted = Prism.highlight(code.trim(), grammar, language);
    
    return highlighted;
  } catch (error) {
    console.warn('Prism highlighting failed:', error);
    // Fallback to plain text with HTML escaping
    return code
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;');
  }
};

/**
 * Converts basic Markdown to HTML with Prism.js syntax highlighting
 */
export const markdownToHtml = (markdown: string): string => {
  // First, extract and process code blocks
  let html = markdown.replace(
    /```(\w+)?\n?([\s\S]*?)```/g,
    (_, language, code) => {
      const lang = language || 'javascript';
      const highlightedCode = highlightCode(code.trim(), lang);
      return `<pre class="prism-code"><code class="language-${lang}">${highlightedCode}</code></pre>`;
    }
  );

  // Then process the rest of the markdown
  html = html
    // Headers
    .replace(/^### (.*$)/gim, '<h3 class="text-lg font-semibold text-git-text-primary mb-2">$1</h3>')
    .replace(/^## (.*$)/gim, '<h2 class="text-xl font-semibold text-git-text-primary mb-3">$1</h2>')
    .replace(/^# (.*$)/gim, '<h1 class="text-2xl font-bold text-git-text-primary mb-4">$1</h1>')

    // Inline code
    .replace(/`([^`]+)`/g, '<code class="inline-code">$1</code>')

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
    .replace(/\n/g, "<br>")

    // Wrap in paragraph tags
    .replace(/^/, '<p class="text-git-text-secondary mb-3">')
    .replace(/$/, "</p>");

  return html;
};

/**
 * Gets the API README content as HTML
 */
export const getAPIReadmeAsHtml = async (): Promise<string> => {
  try {
    const markdown = await fetchAPIReadme();
    return markdownToHtml(markdown);
  } catch (error) {
    console.error("Error processing API README:", error);
    return '<p class="text-git-text-secondary">Unable to load API documentation. Please try again later.</p>';
  }
};
