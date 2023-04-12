import sys
sys.path.append('..')
from test_suite import CheckIfFile

if __name__ == '__main__':
    (
        CheckIfFile('nf_output_res.dat', verbose_on_passed=True).with_split(' ')
            .line(2).at(1).cast(int).match(1)
            .line(5).at(1).cast(float).match_range(-327, -326.8)
            .line(11).at(1).cast(float).match_range(0.08, 0.10)
            .line(12).at(1).cast(float).match_range(455, 476)
            .line(13).at(1).cast(float).match_range(380, 400)
            .line(14).at(1).cast(float).match_range(415, 435)
            .line(15).at(1).cast(float).match_range(470, 490)
            .line(16).at(1).cast(float).match_range(159, 161)
            .line(17).at(1).cast(float).match_range(16, 19)
            .line(18).at(1).cast(float).match_range(64, 66)
            .line(19).at(1).cast(float).match_range(60, 62)
            .line(20).at(1).cast(float).match_range(15, 17)
    )
    (
        CheckIfFile('nf_output_points.paramnames', verbose_on_passed=True)
            .line(1).match('bg', verbatim=False)

            .line(2).match('x1', verbatim=False)
            .line(3).match('x2', verbatim=False)
            .line(4).match('x3', verbatim=False)
            .line(5).match('x4', verbatim=False)

            .line(6).match('amp1', verbatim=False)
            .line(7).match('amp2', verbatim=False)
            .line(8).match('amp3', verbatim=False)
            .line(9).match('amp4', verbatim=False)

            .line(10).match('sigma', verbatim=False)
    )
