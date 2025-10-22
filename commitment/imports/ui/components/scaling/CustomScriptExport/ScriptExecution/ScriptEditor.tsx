import React from 'react';
import Editor from 'react-simple-code-editor';
import useScriptDropzone from './useScriptDropzone';
import {cn} from "@ui/lib/utils";

import Prism from 'prismjs';
import 'prismjs/components/prism-python';
import 'prismjs/components/prism-markup';
import 'prismjs/components/prism-json';

Prism.languages.csv = {
  'string': /"[^"]*"/,
  'number': /\b\d+(\.\d+)?\b/,
  'punctuation': /,/,
  'keyword': /\b[A-Za-z_][A-Za-z0-9_]*\b/
};


export interface ScriptEditorProps {
  className?: string,
  code: string,
  setCode: (code: string) => void,
  onFocus?: () => void,
  onBlur?: () => void,
  setDragging?: (dragging: boolean) => void,
  readonly?: boolean,
  language?: string,
  padding?: number,
}

/**
 * Simple code editor for the python scaling scripts
 * @constructor
 */
export default function ScriptEditor(props: ScriptEditorProps) {
  const language = props.language ?? "python";
  const highlightWithLineNumbers = (code: string) => {
    return Prism.highlight(code, Prism.languages[language], language);
  };

  // Allow files to be drag and dropped in
  const dragProps = useScriptDropzone((files: File[]) => {
    files[files.length - 1].text().then(text => {
      props.setCode(text);
      props.setDragging?.(false);
    });
  });

  return (
    <Editor
      value={props.code}
      onValueChange={code => props.setCode(code)}
      highlight={highlightWithLineNumbers}
      padding={props.padding ?? 10}
      style={{
        outline: 'none',
        border: 'none',
        boxShadow: 'none'
      }}
      textareaClassName="rounded-md outline-0 border-0"
      onFocus={props.onFocus}
      onBlur={props.onBlur}
      {...dragProps}
      onDragOver={e => {
        e.preventDefault();
        props.setDragging?.(true);
      }}
      onDragLeave={() => {
        props.setDragging?.(false);
      }}
      readOnly={props.readonly}

      className={cn("text-foreground w-full rounded-md outline-0 border-0", props.className ?? "")}
      tabSize={2}
      insertSpaces={true}
    />
  );

}
