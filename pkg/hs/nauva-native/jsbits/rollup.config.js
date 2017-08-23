import resolve from 'rollup-plugin-node-resolve'
import commonjs from 'rollup-plugin-commonjs'
import replace from 'rollup-plugin-replace'

export default {
  input: __dirname + '/../../../js/build/js/jsbits/nauva-native/index.js',
  output: {
    file: __dirname + '/index.js',
    format: 'iife',
    name: 'nv$app'
  },
  plugins: [
    resolve({ jsnext: true, main: true }),
    commonjs({
      namedExports: {
        'react': ['Component', 'createElement'],
        'react-dom': ['render'],
      },
    }),
    replace({
      'process.env.NODE_ENV': JSON.stringify('development'),
    }),
  ],
}
