const HtmlWebpackPlugin = require('html-webpack-plugin')
const CopyWebpackPlugin = require('copy-webpack-plugin')

module.exports = {
    entry: `${__dirname}/src/index.js`,
    output: {
        path: `${__dirname}/dist`,
        filename: 'bundle-[contenthash].js',
        libraryTarget: 'window',
    },
    optimization: {
        minimize: true,
    },
    module: {
        rules: [
            {
                test: /\.(css|scss)$/,
                loader: ['style-loader', 'css-loader', 'sass-loader'],
            },
            {
                test: /\.svg$/,
                loader: 'url-loader'
            }
        ],
    },
    plugins: [
        new HtmlWebpackPlugin({ template: `${__dirname}/src/index.html` }),
        new CopyWebpackPlugin([{ from: 'src/assets', to: 'assets' }])
    ],
    mode: process.env.WEBPACK_SERVE ? 'development' : 'production',
    devServer: {
        port: '8080',
        compress: true,
        watchContentBase: true,
        open: 'Google Chrome',
    }
}
