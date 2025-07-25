# Git Colors & Dark Mode Guide

This guide explains how to use custom Git-themed colors in your Tailwind CSS components and implement dark mode functionality.

## 📋 Table of Contents
- [Understanding Custom Colors](#-understanding-custom-colors)
- [Using Git Colors in Components](#️-using-git-colors-in-components)
- [Dark Mode Implementation](#-dark-mode-implementation)
- [Color Categories](#-color-categories)
- [Best Practices](#-best-practices)
- [Examples](#-examples)

## 🎨 Understanding Custom Colors

The Git color system extends Tailwind CSS with custom CSS variables that automatically adapt to light and dark themes. These colors are defined as CSS custom properties and can be used just like regular Tailwind colors.

### How It Works
```css
/* CSS Variables (already defined in your stylesheet) */
--color-git-bg-primary: var(--git-bg-primary);
--color-git-text-primary: var(--git-text-primary);
```

These variables automatically change their values based on the current theme (light/dark), so you don't need to manually specify different colors for each theme.

## 🛠️ Using Git Colors in Components

### Basic Syntax
Use Git colors in your Tailwind classes by referencing them with the `git-` prefix:

```html
<!-- Background colors -->
<div className="bg-git-bg-primary">Primary background</div>
<div className="bg-git-bg-secondary">Secondary background</div>

<!-- Text colors -->
<p className="text-git-text-primary">Primary text</p>
<p className="text-git-text-secondary">Secondary text</p>

<!-- Border colors -->
<div className="border border-git-stroke-primary">Bordered element</div>
```

### Data Visualization Colors
For charts, graphs, and visual elements:

```html
<div className="bg-git-dv-blue">Blue data point</div>
<div className="bg-git-dv-orange">Orange data point</div>
<div className="bg-git-dv-green">Green data point</div>
```

### Interactive Elements
For buttons, links, and interactive components:

```html
<!-- Primary button -->
<button className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover">
  Click me
</button>

<!-- Destructive button -->
<button className="bg-git-int-destructive text-white hover:bg-git-int-destructive-hover">
  Delete
</button>

<!-- Link -->
<a href="#" className="text-git-int-link hover:underline">Learn more</a>
```

## 🌙 Dark Mode Implementation

Add the `dark` className to your root HTML element to activate dark mode:

```html
<!-- Light mode (default) -->
<div classNameName="">
  <!-- Your content -->
</div>

<!-- Dark mode -->
<div classNameName="dark">
  <!-- Your content -->
</div>
```



## 🎯 Color Categories

### Background Colors
- `bg-git-bg-primary` - Main background color
- `bg-git-bg-secondary` - Secondary background color
- `bg-git-bg-tertiary` - Tertiary background color
- `bg-git-bg-elevated` - Elevated elements (modals, dropdowns)
- `bg-git-bg-bottom` - Bottom/footer backgrounds

### Text Colors
- `text-git-text-primary` - Primary text (headings, important text)
- `text-git-text-secondary` - Secondary text (descriptions, metadata)
- `text-git-text-disabled` - Disabled/muted text

### Interactive Colors
- `bg-git-int-primary` - Primary buttons and interactive elements
- `text-git-int-text` - Text on interactive elements
- `bg-git-int-destructive` - Destructive actions (delete, remove)
- `text-git-int-link` - Links and clickable text

### Card Colors
- `bg-git-card-primary` - Card backgrounds
- `bg-git-card-secondary` - Secondary card backgrounds
- `text-git-card-text` - Text on cards
- `text-git-card-text-secondary` - Secondary text on cards

### Border/Stroke Colors
- `border-git-stroke-primary` - Primary borders
- `border-git-stroke-secondary` - Secondary borders
- `border-git-stroke-tertiary` - Subtle borders

### Data Visualization Colors
Perfect for charts, graphs, and visual data:
- `bg-git-dv-blue`, `bg-git-dv-orange`, `bg-git-dv-green`
- `bg-git-dv-coral`, `bg-git-dv-aqua`, `bg-git-dv-yellow`
- `bg-git-dv-purple`, `bg-git-dv-pink`, `bg-git-dv-brown`
- `bg-git-dv-grey`, `bg-git-dv-pink-dark`

## ✨ Best Practices

### 1. Stick to the Color System
Use Git colors for consistency across your application:

```html
<!-- ✅ Good -->
<div className="bg-git-bg-primary text-git-text-primary">
  Content
</div>

<!-- ❌ Avoid mixing systems unnecessarily -->
<div className="bg-gray-100 text-git-text-primary">
  Content
</div>
```

### 2. Use Semantic Color Names
Choose colors based on their purpose, not their appearance:

```html
<!-- ✅ Good - semantic usage -->
<button className="bg-git-int-primary">Save</button>
<button className="bg-git-int-destructive">Delete</button>

<!-- ❌ Avoid - appearance-based usage -->
<button className="bg-git-dv-blue">Save</button>
```

### 3. Test Both Themes
Always test your components in both light and dark modes:

```html
<!-- Add this to your dev tools for easy testing -->
<button onclick="document.documentElement.classList.toggle('dark')">
  Toggle Dark Mode
</button>
```

### 4. Layer Colors Appropriately
Use the elevation system for proper visual hierarchy:

```html
<div className="bg-git-bg-primary">
  <div className="bg-git-bg-secondary">
    <div className="bg-git-bg-elevated">
      <!-- Proper layering -->
    </div>
  </div>
</div>
```

## 📚 Examples

### Complete Card Component
```html
<div className="bg-git-card-primary border border-git-stroke-primary rounded-lg p-6">
  <h3 className="text-git-card-text text-xl font-semibold mb-2">
    Repository Stats
  </h3>
  <p className="text-git-card-text-secondary mb-4">
    View your repository analytics and insights
  </p>
  <div className="flex gap-2">
    <button className="bg-git-int-primary text-git-int-text px-4 py-2 rounded hover:bg-git-int-primary-hover">
      View Details
    </button>
    <button className="bg-git-int-destructive text-white px-4 py-2 rounded hover:bg-git-int-destructive-hover">
      Delete
    </button>
  </div>
</div>
```

### Navigation Bar
```html
<nav className="bg-git-bg-elevated border-b border-git-stroke-secondary">
  <div className="max-w-7xl mx-auto px-4">
    <div className="flex justify-between items-center h-16">
      <div className="text-git-text-primary font-bold text-xl">
        Git Dashboard
      </div>
      <div className="flex gap-4">
        <a href="#" className="text-git-int-link hover:underline">
          Repositories
        </a>
        <a href="#" className="text-git-int-link hover:underline">
          Settings
        </a>
        <button onclick="toggleDarkMode()" className="bg-git-int-primary text-git-int-text px-3 py-1 rounded text-sm">
          Toggle Theme
        </button>
      </div>
    </div>
  </div>
</nav>
```

### Data Visualization Chart
```html
<div className="bg-git-card-primary p-6 rounded-lg">
  <h3 className="text-git-card-text text-lg font-semibold mb-4">
    Commit Activity
  </h3>
  <div className="flex gap-1 h-20 items-end">
    <div className="bg-git-dv-blue w-8 h-12"></div>
    <div className="bg-git-dv-green w-8 h-16"></div>
    <div className="bg-git-dv-orange w-8 h-8"></div>
    <div className="bg-git-dv-coral w-8 h-20"></div>
    <div className="bg-git-dv-purple w-8 h-14"></div>
  </div>
</div>
```
