const path = require('path');

module.exports = {
  mode: 'development',
  entry: './entry.js',
  output: {
    filename: 'index.js',
    path: path.resolve(__dirname, 'public'),
  },
  watchOptions: {
    aggregateTimeout: 1000,
    poll: 1000,
  },
  devServer: {
    static: './public',
    open: true,
    compress: false,
    port: 8081,
  },
  plugins: [
  ]
};