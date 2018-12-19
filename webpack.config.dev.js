const merge = require('webpack-merge');
const commonConfig = require('./webpack.config.common.js');

module.exports = merge(commonConfig, {
    module: {
        rules: [
            {
                test:    /\.elm$/,
                loader: 'elm-webpack-loader',
                options: {
                    debug: true
                }
            },
        ],
    },
    mode: 'development',
    devServer: {
        port: '8080',
        compress: true,
        watchContentBase: true,
        open: 'Google Chrome',
    }
})
