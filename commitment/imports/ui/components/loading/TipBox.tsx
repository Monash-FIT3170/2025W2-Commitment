import React, { FC } from 'react';

export interface TipBoxProps {
  tip: string;
}

const TipBox: FC<TipBoxProps> = ({ tip }) => (
  <p className="mt-6 text-inconsolata-italic">
    {tip}
  </p>
);

export default TipBox;