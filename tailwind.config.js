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
    },
    colors: {
      transparent: 'transparent',
      current: 'currentColor',
      yukiRed: {
        DEFAULT: '#A8233E',
      },
      yukiYellow: {
        DEFAULT: '#F5F1E7',
      },
      yukiBlack: {
        DEFAULT: '#323232',
      },
      white: {
        DEFAULT: '#FFFFFF'
      }
    }
  },
  variants: {
    extend: {},
  },
  plugins: [],
}