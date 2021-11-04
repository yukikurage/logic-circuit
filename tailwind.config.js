module.exports = {
  mode: 'jit',
  // These paths are just examples, customize them to match your project structure
  purge: [
    './public/index.html',
    './src/**/*.purs',
  ],
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {
      fontFamily: {
        'meiryo': ['Meiryo','メイリオ', 'sans-serif'],
        'math': ['Vollkorn', 'serif']
      },
      animation: {
        'fade-in-quick': 'fade-in-keyframes 50ms linear 1',
      },
      keyframes: {
        'fade-in-keyframes': {
          '0%': {opacity: '0', top: '-0px'},
          '100%': {opacity: '1', top: '0px'},
        },
      },
    },
    colors: {
      transparent: 'transparent',
      current: 'currentColor',
      yukiRed: {
        DEFAULT: '#A8233E',
      },
      yukiYellow: {
        DEFAULT: '#f2f1ed',
      },
      yukiBlack: {
        DEFAULT: '#323232',
      },
      white: {
        DEFAULT: '#FFFFFF'
      }
    },
    boxShadow: {
      'darker-md': '0 6px 0px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.4)',
      sm: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
      DEFAULT: '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)',
      md: '0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)',
      lg: '0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05)',
      xl: '0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)',
      '2xl': '0 25px 50px -12px rgba(0, 0, 0, 0.25)',
      '3xl': '0 35px 60px -15px rgba(0, 0, 0, 0.3)',
      inner: 'inset 0 2px 4px 0 rgba(0, 0, 0, 0.06)',
      none: 'none',
    }
  },
  variants: {
    extend: {},
  },
  plugins: [],
}