import sys
sys.path.append('..')
from test_suite import CheckIfFile

if __name__ == '__main__':
    (
        CheckIfFile('nf_output_res.dat', verbose_on_passed=True).with_split(' ')
            .line(2).at(1).cast(int).match(1)
            .line(11).at(1).cast(float).match_range(0.10, 0.12)
            .line(12).at(1).cast(float).match_range(453, 456)
            .line(13).at(1).cast(float).match_range(284, 316)
            .line(14).at(1).cast(float).match_range(24, 28)
    )
    (
        CheckIfFile('nf_output_points.paramnames', verbose_on_passed=True)
            .line(1).match('bg', verbatim=False)
            .line(2).match('x0', verbatim=False)
            .line(3).match('amp', verbatim=False)
            .line(4).match('sigma', verbatim=False)
    )
