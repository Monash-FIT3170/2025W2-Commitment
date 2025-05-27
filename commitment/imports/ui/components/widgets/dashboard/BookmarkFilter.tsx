import React from 'react'
import Filter, { FilterOption, FiltersState, FilterValue } from '../../ui/filter'


interface FilterProps {
  filters: FiltersState;
  onFilterChange: (key: string, value: FilterValue) => void;
}

export default function BookmarkFilter({filters,onFilterChange}:FilterProps) {


    const options: FilterOption[] = [{
        type: "date",
        label: "Created At",
        filterkey: 'createdAt'
      },{
        type: "date",
        label: "Last Viewed",
        filterkey: 'lastViewed'
      }]
  return (
    <Filter filters={filters} onFilterChange={onFilterChange} options={options}/>
  )
}
