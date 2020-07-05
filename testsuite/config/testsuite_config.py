# vim: set filetype=python:

import re

class TestsuiteConfig:
    @staticmethod
    def init_config(config: TestConfig)
        raise NotImplemented

    @staticmethod
    def get_compiler_info(config: TestConfig) -> None:
        raise NotImplemented

class GHCTestsuiteConfig(TestsuiteConfig):
    """
    Testsuite configuration setup for GHC
    """

    @staticmethod
    def init_config(config: TestConfig):
        config.compiler_always_flags = ghc_compiler_always_flags.split()

        # By default, the 'normal' and 'hpc' ways are enabled. In addition, certain
        # ways are enabled automatically if this GHC supports them. Ways that fall in
        # this group are 'optasm', 'optllvm', 'profasm', 'threaded1', 'threaded2',
        # 'profthreaded', 'ghci', and whichever of 'static/dyn' is not this GHC's
        # default mode. Other ways should be set explicitly from .T files.
        config.compile_ways       = ['normal', 'hpc']
        config.run_ways           = ['normal', 'hpc']

        # ways that are not enabled by default, but can always be invoked explicitly
        config.other_ways         = ['prof', 'normal_h',
                                    'prof_hc_hb','prof_hb',
                                    'prof_hd','prof_hy','prof_hr',
                                    'sanity',
                                    'threaded1_ls', 'threaded2_hT', 'debug_numa',
                                    'llvm', 'debugllvm',
                                    'profllvm', 'profoptllvm', 'profthreadedllvm',
                                    'debug',
                                    'ghci-ext', 'ghci-ext-prof',
                                    'ext-interp',
                                    'nonmoving',
                                    'nonmoving_thr',
                                    'nonmoving_thr_ghc',
                                    'compacting_gc',
                                    ]

        if ghc_with_native_codegen:
            config.compile_ways.append('optasm')
            config.run_ways.append('optasm')

        if config.have_profiling:
            config.compile_ways.append('profasm')
            config.run_ways.append('profasm')

        if config.have_interp:
            config.run_ways.append('ghci')

        if ghc_with_threaded_rts:
            config.run_ways.append('threaded1')
            if ghc_with_smp:
                config.have_smp = True
                config.run_ways.append('threaded2')
                if config.speed == 0:
                    config.run_ways.append('nonmoving_thr')

        if ghc_with_dynamic_rts:
            config.have_shared_libs = True

        if config.ghc_dynamic_by_default and config.have_vanilla == 1:
            config.run_ways.append('static')
        else:
            if ghc_with_dynamic_rts:
                config.run_ways.append('dyn')

        if (config.have_profiling and ghc_with_threaded_rts):
            config.run_ways.append('profthreaded')

        if (ghc_with_llvm and not config.unregisterised):
            config.compile_ways.append('optllvm')
            config.run_ways.append('optllvm')

        config.way_flags = {
            'normal'       : [],
            'normal_h'     : [],
            'g1'           : [],
            'nursery_chunks' : [],
            'debug_numa'   : ['-threaded', '-debug'],
            'optasm'       : ['-O', '-fasm'],
            'llvm'         : ['-fllvm'],
            'optllvm'      : ['-O', '-fllvm'],
            'debugllvm'    : ['-fllvm', '-keep-llvm-files'],
            'prof'         : ['-prof', '-static', '-fprof-auto', '-fasm'],
            'prof_no_auto' : ['-prof', '-static', '-fasm'],
            'profasm'      : ['-O', '-prof', '-static', '-fprof-auto'],
            'profthreaded' : ['-O', '-prof', '-static', '-fprof-auto', '-threaded'],
            'ghci'         : ['--interactive', '-v0', '-ignore-dot-ghci', '-fno-ghci-history', '+RTS', '-I0.1', '-RTS'] + (['-fghci-leak-check'] if not config.compiler_debugged else []),
            'sanity'       : ['-debug'],
            'threaded1'    : ['-threaded', '-debug'],
            'threaded1_ls' : ['-threaded', '-debug'],
            'threaded2'    : ['-O', '-threaded', '-eventlog'],
            'threaded2_hT' : ['-O', '-threaded'],
            'hpc'          : ['-O', '-fhpc'],
            'prof_hc_hb'   : ['-O', '-prof', '-static', '-fprof-auto'],
            'prof_hb'      : ['-O', '-prof', '-static', '-fprof-auto'],
            'prof_hd'      : ['-O', '-prof', '-static', '-fprof-auto'],
            'prof_hy'      : ['-O', '-prof', '-static', '-fprof-auto'],
            'prof_hr'      : ['-O', '-prof', '-static', '-fprof-auto'],
            'dyn'          : ['-O', '-dynamic'],
            'static'       : ['-O', '-static'],
            'debug'        : ['-O', '-g', '-dannot-lint'],
            # llvm variants...
            'profllvm'         : ['-prof', '-static', '-fprof-auto', '-fllvm'],
            'profoptllvm'      : ['-O', '-prof', '-static', '-fprof-auto', '-fllvm'],
            'profthreadedllvm' : ['-O', '-prof', '-static', '-fprof-auto', '-threaded', '-fllvm'],
            'ghci-ext'         : ['--interactive', '-v0', '-ignore-dot-ghci', '-fno-ghci-history', '-fexternal-interpreter', '+RTS', '-I0.1', '-RTS'],
            'ghci-ext-prof'    : ['--interactive', '-v0', '-ignore-dot-ghci', '-fno-ghci-history', '-fexternal-interpreter', '-prof', '+RTS', '-I0.1', '-RTS'],
            'ext-interp'   : ['-fexternal-interpreter'],
            'nonmoving'    : [],
            'nonmoving_thr': ['-threaded'],
            'nonmoving_thr_ghc': ['+RTS', '-xn', '-N2', '-RTS', '-threaded'],
            'compacting_gc': [],
        }

        config.way_rts_flags = {
            'normal'       : [],
            'normal_h'     : ['-h'], # works without -prof
            'g1'           : ['-G1'],
            'nursery_chunks' : ['-n32k'],
            'debug_numa'   : ['-N2', '--debug-numa=2'],
            'optasm'       : [],
            'llvm'         : [],
            'optllvm'      : [],
            'debugllvm'    : [],
            'prof'         : ['-p'],
            'prof_no_auto' : ['-p'],
            'profasm'      : ['-hc', '-p'], # test heap profiling too
            'profthreaded' : ['-p'],
            'ghci'         : [],
            'sanity'       : ['-DS'],
            'threaded1'    : [],
            'threaded1_ls' : ['-ls'],
            'threaded2'    : ['-N2', '-ls'],
            'threaded2_hT' : ['-N2', '-hT'],
            'hpc'          : [],
            'prof_hc_hb'   : ['-hc', '-hbvoid'],
            'prof_hb'      : ['-hb'],
            'prof_hd'      : ['-hd'],
            'prof_hy'      : ['-hy'],
            'prof_hr'      : ['-hr'],
            'dyn'          : [],
            'static'       : [],
            'debug'        : [],
            # llvm variants...
            'profllvm'         : ['-p'],
            'profoptllvm'      : ['-hc', '-p'],
            'profthreadedllvm' : ['-p'],
            'ghci-ext'         : [],
            'ghci-ext-prof'    : [],
            'ext-interp'       : [],
            'nonmoving'        : ['-xn'],
            'nonmoving_thr'    : ['-xn', '-N2'],
            'nonmoving_thr_ghc': ['-xn', '-N2'],
            'compacting_gc': ['-c'],
        }

        # Useful classes of ways that can be used with only_ways(), omit_ways() and
        # expect_broken_for().

        prof_ways     = [x[0] for x in config.way_flags.items()
                            if '-prof' in x[1]]

        threaded_ways = [x[0] for x in config.way_flags.items()
                            if '-threaded' in x[1] or 'ghci' == x[0]]

        # Ways which run with multiple capabilities
        concurrent_ways = [name for name, flags in config.way_flags.items()
                                if '-threaded' in flags or 'ghci' == name
                                if '-N2' in config.way_rts_flags.get(name, [])]

        opt_ways      = [x[0] for x in config.way_flags.items()
                            if '-O' in x[1]]

        llvm_ways     = [x[0] for x in config.way_flags.items()
                            if '-fflvm' in x[1]]

    @staticmethod
    def get_compiler_info():
        s = getStdout([config.compiler, '--info'])
        s = re.sub('[\r\n]', '', s)
        compilerInfoDict = dict(eval(s))
        s = getStdout([config.compiler, '+RTS', '--info'])
        s = re.sub('[\r\n]', '', s)
        rtsInfoDict = dict(eval(s))

        config.have_ncg = compilerInfoDict.get("Have native code generator", "NO") == "YES"

        # Whether GHC itself was built using the LLVM backend. We need to know this
        # since some tests in ext-interp fail when stage2 ghc is built using
        # LLVM. See #16087.
        #
        # The condition here is a bit approximate: we assume that if stage2 doesn't
        # have the NCG and isn't unregisterised then it must be using the LLVM
        # backend by default.
        config.ghc_built_by_llvm = not config.have_ncg and not config.unregisterised

        config.have_RTS_linker = compilerInfoDict.get("target has RTS linker", "NO") == "YES"
        # external interpreter needs RTS linker support
        # If the field is not present (GHC 8.0 and earlier), assume we don't
        # have -fexternal-interpreter (though GHC 8.0 actually does)
        # so we can still run most tests.
        config.have_ext_interp = compilerInfoDict.get("target has RTS linker", "NO") == "YES"

        # See Note [Replacing backward slashes in config.libdir].
        config.libdir = compilerInfoDict['LibDir'].replace('\\', '/')

        if re.match(".*_p(_.*|$)", rtsInfoDict["RTS way"]):
            config.compiler_profiled = True
        else:
            config.compiler_profiled = False

        try:
            config.package_conf_cache_file = compilerInfoDict["Global Package DB"] + '/package.cache'
        except:
            config.package_conf_cache_file = ''

        # See Note [WayFlags]
        if config.ghc_dynamic:
            config.ghc_th_way_flags = "-dynamic"
            config.ghci_way_flags   = "-dynamic"
            config.plugin_way_flags = "-dynamic"
            config.ghc_th_way       = "dyn"
            config.ghc_plugin_way   = "dyn"
        elif config.compiler_profiled:
            config.ghc_th_way_flags = "-prof"
            config.ghci_way_flags   = "-prof"
            config.plugin_way_flags = "-prof"
            config.ghc_th_way       = "prof"
            config.ghc_plugin_way   = "prof"
        else:
            config.ghc_th_way_flags = "-static"
            config.ghci_way_flags   = "-static"
            config.plugin_way_flags = "-static"
            config.ghc_th_way       = "normal"
            config.ghc_plugin_way   = "normal"

# Note [Replacing backward slashes in config.libdir]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# We *do* need to replace backslashes in config.libdir, for the following
# reason:
#
# * Tests use config.libdir as follows:
#
#     extra_run_opts('"' + config.libdir + '"')
#
#   The double quotes are there because config.libdir might contain
#   spaces.
#
# * This string is then written /as is/ to <testname>.genscript in
#   testlib.interpreter_run:
#
#     script.write(':set args ' + opts.extra_run_opts + '\n')
#
# * But GHCi expects the arguments to ':set args' to be proper Haskell
#   strings (when they are quoted), with backslashes escaped. Since
#   config.libdir contains single backslash characters, tests such as T5313
#   will fail for WAY=ghci with "Pattern match failure in do expression".
#
# Arguably the above code for writing `:set args` should be smarter. This
# is tricky to get right though, because in GHCI `:set args foo\bar` (no
# double quotes) works perfectly fine, and is interpreted as the Haskell
# string "foo\\bar". Therefore, simply escaping all backward slashes in
# opts.extra_run_opts before concatenating it with ':set args' is not right
# either.
#
# Replacing backslashes to forward slashes in config.libdir works around the
# problem.
