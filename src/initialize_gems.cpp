// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <fstream>
#include <string>
#include <vector>

#include <sparsepp/spp.h>
//[[Rcpp::depends(sparsepp)]]

typedef spp::sparse_hash_map<std::string, std::string> gem;

// gem for year 2017
gem forward_map_2017;
gem backward_map_2017;
gem reverse_forward_map_2017;
gem reverse_backward_map_2017;

// gem for year 2018
gem forward_map_2018;
gem backward_map_2018;
gem reverse_forward_map_2018;
gem reverse_backward_map_2018;

// generate dictionary
inline gem dict_str(const std::vector<std::string>& keys,
                    const std::vector<std::string>& values)
{
    // create an unordered_map
    gem dx;
    for (size_t i {0}; i < keys.size(); ++i) {
        // insert a new pair key and value
        dx.insert(std::make_pair(keys[i], values[i]));
    }
    return dx;
}

// [[Rcpp::export]]
void init_gem_f17(const std::vector<std::string>& keys,
                  const std::vector<std::string>& values)
{
    forward_map_2017 = dict_str(keys, values);
}

// [[Rcpp::export]]
void init_gem_b17(const std::vector<std::string>& keys,
                  const std::vector<std::string>& values)
{
    backward_map_2017 = dict_str(keys, values);
}

// [[Rcpp::export]]
void init_gem_rf17(const std::vector<std::string>& keys,
                   const std::vector<std::string>& values)
{
    reverse_forward_map_2017 = dict_str(keys, values);
}

// [[Rcpp::export]]
void init_gem_rb17(const std::vector<std::string>& keys,
                   const std::vector<std::string>& values)
{
    reverse_backward_map_2017 = dict_str(keys, values);
}

// [[Rcpp::export]]
void init_gem_f18(const std::vector<std::string>& keys,
                  const std::vector<std::string>& values)
{
    forward_map_2018 = dict_str(keys, values);
}

// [[Rcpp::export]]
void init_gem_b18(const std::vector<std::string>& keys,
                  const std::vector<std::string>& values)
{
    backward_map_2018 = dict_str(keys, values);
}

// [[Rcpp::export]]
void init_gem_rf18(const std::vector<std::string>& keys,
                   const std::vector<std::string>& values)
{
    reverse_forward_map_2018 = dict_str(keys, values);
}

// [[Rcpp::export]]
void init_gem_rb18(const std::vector<std::string>& keys,
                   const std::vector<std::string>& values)
{
    reverse_backward_map_2018 = dict_str(keys, values);
}
