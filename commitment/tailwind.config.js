/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./client/**/*.{js,ts,jsx,tsx}",
    "./imports/**/*.{js,ts,jsx,tsx}",
    "./public/**/*.html",
  ],
  theme: {
    extend: {
      colors: {
        // Git Background Colors
        'git-bg-bottom': 'var(--git-bg-bottom)',
        'git-bg-primary': 'var(--git-bg-primary)',
        'git-bg-secondary': 'var(--git-bg-secondary)',
        'git-bg-tertiary': 'var(--git-bg-tertiary)',
        'git-bg-elevated': 'var(--git-bg-elevated)',
        
        // Git Text Colors
        'git-text-primary': 'var(--git-text-primary)',
        'git-text-secondary': 'var(--git-text-secondary)',
        'git-text-disabled': 'var(--git-text-disabled)',
        
        // Git Data Visualization Colors
        'git-dv-blue': 'var(--git-dv-blue)',
        'git-dv-orange': 'var(--git-dv-orange)',
        'git-dv-green': 'var(--git-dv-green)',
        'git-dv-coral': 'var(--git-dv-coral)',
        'git-dv-aqua': 'var(--git-dv-aqua)',
        'git-dv-yellow': 'var(--git-dv-yellow)',
        'git-dv-purple': 'var(--git-dv-purple)',
        'git-dv-pink': 'var(--git-dv-pink)',
        'git-dv-brown': 'var(--git-dv-brown)',
        'git-dv-grey': 'var(--git-dv-grey)',
        'git-dv-pink-dark': 'var(--git-dv-pink-dark)',
        
        // Git Interactive Colors
        'git-int-primary': 'var(--git-int-primary)',
        'git-int-text': 'var(--git-int-text)',
        'git-int-destructive': 'var(--git-int-destructive)',
        'git-int-primary-hover': 'var(--git-int-primary-hover)',
        'git-int-destructive-hover': 'var(--git-int-destructive-hover)',
        'git-int-link': 'var(--git-int-link)',
        
        // Git Card Colors
        'git-card-primary': 'var(--git-card-primary)',
        'git-card-secondary': 'var(--git-card-secondary)',
        'git-card-text': 'var(--git-card-text)',
        'git-card-text-secondary': 'var(--git-card-text-secondary)',
        
        // Git Stroke Colors
        'git-stroke-primary': 'var(--git-stroke-primary)',
        'git-stroke-secondary': 'var(--git-stroke-secondary)',
        'git-stroke-tertiary': 'var(--git-stroke-tertiary)',
        
        // Git Tabs Colors
        'git-tabs-hovered': 'var(--git-tabs-hovered)',
        'git-tabs-active': 'var(--git-tabs-active)',
        
        // Primitive Colors
        cararra: {
          50: '#f0f0e8',
          100: '#e8e8dd',
          200: '#d0d0b7',
          300: '#b7b592',
          400: '#a6a379',
          500: '#867a59',
        },
        zeus: {
          600: '#5c5c51',
          700: '#42423a',
          750: '#393932',
          800: '#2f2f2a',
          850: '#282823',
          900: '#252522',
          950: '#21211e',
        },
        ivory: {
          100: '#e8e8dd',
        },
        git: {
          50: '#fef3ee',
          100: '#ffeae1',
          200: '#ffdac9',
          300: '#febfa3',
          400: '#fa9a6f',
          500: '#f1502f',
          600: '#e12a15',
          700: '#bb1c13',
          800: '#951917',
          900: '#781816',
          950: '#41090a',
        },
        
        // Shadcn Colors
        border: 'var(--border)',
        input: 'var(--input)',
        ring: 'var(--ring)',
        background: 'var(--background)',
        foreground: 'var(--foreground)',
        primary: {
          DEFAULT: 'var(--primary)',
          foreground: 'var(--primary-foreground)',
        },
        secondary: {
          DEFAULT: 'var(--secondary)',
          foreground: 'var(--secondary-foreground)',
        },
        destructive: {
          DEFAULT: 'var(--destructive)',
          foreground: 'var(--destructive-foreground)',
        },
        muted: {
          DEFAULT: 'var(--muted)',
          foreground: 'var(--muted-foreground)',
        },
        accent: {
          DEFAULT: 'var(--accent)',
          foreground: 'var(--accent-foreground)',
        },
        popover: {
          DEFAULT: 'var(--popover)',
          foreground: 'var(--popover-foreground)',
        },
        card: {
          DEFAULT: 'var(--card)',
          foreground: 'var(--card-foreground)',
        },
      },
      borderRadius: {
        lg: 'var(--radius)',
        md: 'calc(var(--radius) - 2px)',
        sm: 'calc(var(--radius) - 4px)',
      },
      fontFamily: {
        sans: [
          'ui-sans-serif',
          'system-ui',
          'sans-serif',
          '"Apple Color Emoji"',
          '"Segoe UI Emoji"',
          '"Segoe UI Symbol"',
          '"Noto Color Emoji"',
        ],
        mono: [
          'ui-monospace',
          'SFMono-Regular',
          'Menlo',
          'Monaco',
          'Consolas',
          '"Liberation Mono"',
          '"Courier New"',
          'monospace',
        ],
      },
      keyframes: {
        "accordion-down": {
          from: { height: "0" },
          to: { height: "var(--radix-accordion-content-height)" },
        },
        "accordion-up": {
          from: { height: "var(--radix-accordion-content-height)" },
          to: { height: "0" },
        },
      },
      animation: {
        "accordion-down": "accordion-down 0.2s ease-out",
        "accordion-up": "accordion-up 0.2s ease-out",
      },
    },
  },
  plugins: [require("tailwindcss-animate")],
}
