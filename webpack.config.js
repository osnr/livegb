var path = require('path');

module.exports = {
  devtool: 'cheap-eval-source-map',
  entry: {
    main: './lib/index.ts'
  },
  output: {
    path: path.resolve(__dirname, './dist'),
    filename: '[name].js',
    chunkFilename: '[chunkhash].js'
  },
  module: {
    rules: [{
      test: /\.ts(x?)$/,
      exclude: /node_modules/,
      use: [ { loader: 'ts-loader' } ]
    }]
  },
  plugins: [
  ],
  resolve: {
    extensions: ['.ts', '.tsx', '.js']
  },
};
