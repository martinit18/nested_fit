import sys
sys.path.append('..')
from test_suite import CheckIfFile

if __name__ == '__main__':
    (
        CheckIfFile('nf_output_res.dat', verbose_on_passed=True).with_split(' ')
            .line(2).at(1).cast(int).match(3)
            .line(11).at(1).cast(float).match_range(0.007, 0.008)
            .line(12).at(1).cast(float).match_range(0.003, 0.004)
            .line(13).at(1).cast(float).match_range(0.0015, 0.0025)
            .line(14).at(1).cast(float).match_range(-0.02, -0.01)
            .line(15).at(1).cast(float).match_range(-0.02, -0.01)
    )
