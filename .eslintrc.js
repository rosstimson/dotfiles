module.exports = {
	'env': {
		'node': true,
		'es6': true,
	},
	'extends': 'eslint:recommended',
	'rules': {
		'brace-style': 'error',
		'camelcase': 'error',
		'comma-dangle': ['error', 'always-multiline'],
		'comma-spacing': 'error',
		'comma-style': 'error',
		'curly': 'error',
		'eqeqeq': 'error',
		'indent': ['error', 'tab'],
		'linebreak-style': ['error', 'unix'],
		'no-var': 'error',
		'prefer-const': 'error',
		'quotes': ['error', 'single', 'avoid-escape'],
		'semi': ['error', 'always'],
		'semi-spacing': 'error',
		'semi-style': ['error', 'last'],
		'strict': 'error',
	},
};
