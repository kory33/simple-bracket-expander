const merge = require('webpack-merge');
const commonConfig = require('./webpack.config.common.js');

module.exports = merge(commonConfig, {
    module: {
        rules: [
            {
                test:    /\.elm$/,
                loader: 'elm-webpack-loader',
                options: {
                    debug: false,
                    optimize: true
                }
            },
        ],
    },
    mode: 'production',
})
