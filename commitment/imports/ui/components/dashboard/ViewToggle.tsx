import React from 'react';
import { Switch } from '@base/switch';
import { Label } from '@base/label';

interface ViewToggleProps {
  value: 'list' | 'gallery';
  onChange: (next: 'list' | 'gallery') => void;
}

const ViewToggle: React.FC<ViewToggleProps> = ({
  value,
  onChange,
}) => {
  const isList = value === 'list';
  const label = isList ? 'List' : 'Gallery';

  return (
    <>
      <Switch
        id="view-mode"
        onCheckedChange={() => onChange(isList ? 'gallery' : 'list')}
      />
      <Label className="w-[120px] text-center" htmlFor="view-mode">
        {label}
        {' '}
        Mode
      </Label>
    </>
  );
};

export default ViewToggle;
