#include <Rcpp.h>
#include <cstring>

int index(const int val, Rcpp::IntegerVector vec) {
	int ind = std::find(vec.begin(), vec.end(), val) - vec.begin();
	return ind;
}

int maxneb_disc () {
	Rcpp::IntegerVector targs(targets);
	Rcpp::IntegerVector nebs(neighbors);
	Rcpp::IntegerVector plot(plots);
	Rcpp::CharacterVector status(stat);
	Rcpp::IntegerVector xx(bqudx);
	Rcpp::IntegerVector yy(bqudy);
	Rcpp::IntegerVector tt(time);
	int rad = Rcpp::as<int>(sr);
	Rcpp::IntegerVector counts(targs.size());
	int max_neb = 0;

	for (int i = 0, n = targs.size(); i < n; i++) {
		int targ = index(targs[i], nebs);
		int num_nebs= 0;
		for (int neb = 0, n2 = nebs.size(); neb < n2; neb++) {
			if (targ != neb && plot[targ] == plot[neb] &&
				(std::strcmp("ALIVE", status[neb]) == 0) &&
				(xx[targ] + sr > xx[neb]) && (xx[targ] - sr < xx[neb]) &&
				(yy[targ] + sr > yy[neb]) && (yy[targ] - sr < yy[neb]) &&
				tt[targ] == tt[neb])
				num_nebs++;
		}
		counts[i] = num_nebs;
	}

	return counts;

}
