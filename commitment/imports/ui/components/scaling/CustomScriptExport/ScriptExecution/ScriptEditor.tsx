import React from 'react';
import Editor from 'react-simple-code-editor';
import hljs from 'highlight.js/lib/core';
import python from 'highlight.js/lib/languages/python';

hljs.registerLanguage('python', python);


export interface ScriptEditorProps {
  code: string,
  setCode: (code: string) => void,
}

/**
 * Simple code editor for the python scaling scripts
 * @constructor
 */
export default function ScriptEditor(props: ScriptEditorProps) {

  return (
    <Editor
      value={props.code}
      onValueChange={code => props.setCode(code)}
      highlight={code => hljs.highlight(code, {language: "python"}).value}
      padding={10}
    >

    </Editor>
  )
}
