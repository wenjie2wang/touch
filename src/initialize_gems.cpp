//
// R package touch by Wenjie Wang, Yan Li, and Jun Yan
// Copyright (C) 2015-2019
//
// This file is part of the R package touch.
//
// The R package touch is free software: You can redistribute it and/or
// modify it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or any later
// version (at your option). See the GNU General Public License at
// <https://www.gnu.org/licenses/> for details.
//
// The R package touch is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//

// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <string>
#include <unordered_map>
#include <vector>

typedef std::unordered_map<std::string, std::string> gem;


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
