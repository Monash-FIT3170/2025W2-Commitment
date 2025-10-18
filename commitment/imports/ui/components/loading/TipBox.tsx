import React, { FC } from 'react';

export interface TipBoxProps {
  tip: string;
}

const TipBox: FC<TipBoxProps> = ({ tip }) => (
  <p className="mt-4 md:mt-6 text-git-text-secondary italic text-center">
    {tip}
  </p>
);

export default TipBox;
