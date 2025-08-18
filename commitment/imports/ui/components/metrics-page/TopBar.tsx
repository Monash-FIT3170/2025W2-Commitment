import React from 'react';
import { Settings } from 'lucide-react';

export function TopBar() {
  return (
    <div className="flex items-center justify-between px-10 py-3 border-b border-gray-200 bg-[#FEFEFA]">
      <div className="flex items-center gap-3">
        <h2 className="text-lg font-semibold text-gray-800">Repo Name</h2>
        {/* Bookmark icon placeholder */}
        <div className="w-6 h-6 bg-gray-300 rounded" />
        {' '}
      </div>
      <Settings className="w-5 h-5 text-gray-500 hover:text-gray-700 cursor-pointer" />
    </div>
  );
}
