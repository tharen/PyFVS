
import os
import sys
import argparse
import logging

import pyfvs
from .fvs import FVS

pyfvs.init_logging()
log = logging.getLogger('pyfvs.__main__')

class ListVariantsAction(argparse.Action):
    def __init__(self, option_strings, dest, **kwargs):
        super(ListVariantsAction, self).__init__(option_strings, dest, **kwargs)

    # def __call__(self, parser, namespace, values, option_string=None):
    def __call__(self, *args, **kargs):
        # print('%r %r %r' % (namespace, values, option_string))
        # setattr(namespace, self.dest, values)
        sys.exit(0)

def handle_command_line():
    """
    Return arguments collected from the command line.
    """
    import argparse

    parser = argparse.ArgumentParser(
            description='PyFVS Command Line Interface.'
            , epilog='PyFVS is on GitHub (https://github.com/tharen/PyFVS).')

    parser.add_argument('fvs_variant', nargs='?', type=str
            , metavar='FVS Variant'
            , help='FVS variant to run.')

    parser.add_argument('keyword_file', nargs='?', type=str
            , metavar='Keyword File'
            , help='Path to the FVS keyword file to execute.')

    parser.add_argument('-s', '--stochastic', dest='stochastic'
            , action='store_true', default=False
            , help='Run FVS with stochastic components.')

    parser.add_argument('-b', '--bootstrap', dest='bootstrap'
            , metavar='', type=int, nargs=1, default=0
            , help='(not impl) Bootstrap resampling of sample plot data.')

    parser.add_argument('-d', '--debug', dest='debug'
            , action='store_true', default=False
            , help='Set logging level to debug.')

    parser.add_argument('-p', '--prompt', dest='prompt'
            , action='store_true', default=False
            , help='Prompt the user for I/O files.')

    parser.add_argument('-v', '--version', action='version'
            , version=pyfvs.__version__)

    parser.add_argument('--help-variants', action='store_true', default=False
            , help='List the supported FVS variants.')

    parser.add_argument('--run-tests', action='store_true', default=False
            , help='Run tests against the supported variants.')

    args = parser.parse_args()

    if args.run_tests:
        import subprocess
        os.chdir(os.path.join(os.path.dirname(__file__), 'test'))
        subprocess.call('pytest')
        sys.exit()

    if args.help_variants:
        vars = pyfvs.list_variants()
        msg = 'Supported FVS variants:\n'
        msg += '  ' + '\n  '.join(vars)
        print(msg)
        sys.exit(0)

    if not (args.fvs_variant and args.keyword_file):
        log.error(' '.join(sys.argv) +
                ('\nToo few arguments. A variant and keyword '
                'file are required.\n')
                )
        parser.print_help()
        sys.exit(2)

    return args

def main():
    """
    Execute a FVS projection from the command line.
    
    Basic Usage:
        pyfvs -h
        pyfvs <variant> <keyword file>
        python -m pyfvs <variant> <keyword file>
    """

    args = handle_command_line()

    if args.bootstrap > 0:
        log.info('Bootstrap resampling of plot data is not implemented.')

    if args.debug:
        log.setLevel(logging.DEBUG)

    if args.prompt:
        # Run FVS in prompt mode
        r = FVS(args.fvs_variant).fvs()
        sys.exit(r)

    if not os.path.exists(args.keyword_file):
        msg = 'The keyword file is does not exist: {}'.format(args.keyword_file)
        log.error(msg)
        sys.exit(1)

    # Execute the run
    try:
        fvs = FVS(args.fvs_variant, stochastic=args.stochastic)

    except ImportError:
        log.error((
            'Variant code \'{}\' is not '
            'a supported variant.\n'
            'Supported variants: {}'
            ).format(args.fvs_variant, '; '.join(pyfvs.list_variants()))
            )
        sys.exit(1)

    except:
        sys.exit(1)

    try:
        fvs.execute_projection(args.keyword_file)
    except:
        log.exception('Error running FVS.')
        sys.exit(1)

    print(fvs.outcom_mod.iosum[:6, :fvs.num_cycles + 1].T)
#     print(fvs.get_summary('merch bdft'))

if __name__ == '__main__':
    main()
