import React from 'react';
import Editor from 'react-simple-code-editor';
import Prism from 'prismjs';
import 'prismjs/components/prism-python';

import 'prismjs/themes/prism.css';


export interface ScriptEditorProps {
  code: string,
  setCode: (code: string) => void,
}

/**
 * Simple code editor for the python scaling scripts
 * @constructor
 */
export default function ScriptEditor(props: ScriptEditorProps) {
  const highlightWithLineNumbers = (code: string) => {
    return Prism.highlight(code, Prism.languages.python, 'python');
  };

  return (
    <Editor
      value={props.code}
      onValueChange={code => props.setCode(code)}
      highlight={highlightWithLineNumbers}
      padding={10}
      style={{
        fontFamily: '"Fira code", "Fira Mono", monospace',
        fontSize: 12,
      }}
      className="text-foreground w-full h-full bg-background rounded-md"
      tabSize={2}
      insertSpaces={true}
    />
  );

}
