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

#include <algorithm>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>


typedef std::unordered_map<std::string, std::string> gem;
typedef std::unordered_map<std::string,std::string>::iterator gem_it;

// gem for year 2017
extern gem forward_map_2017;
extern gem backward_map_2017;
extern gem reverse_forward_map_2017;
extern gem reverse_backward_map_2017;
gem frb_map_2017;
gem brf_map_2017;
gem multi_0910_map_2017;
gem multi_1009_map_2017;

// gem for year 2018
extern gem forward_map_2018;
extern gem backward_map_2018;
extern gem reverse_forward_map_2018;
extern gem reverse_backward_map_2018;
gem frb_map_2018;
gem brf_map_2018;
gem multi_0910_map_2018;
gem multi_1009_map_2018;


// utility functions
// =============================================================================
// sort and unique std::vector<std::string>
inline std::vector<std::string> uni_sort(const std::vector<std::string>& x)
{
    std::vector<std::string> out;
    std::unordered_set<std::string> set_x;
    for (std::string i : x)
        set_x.insert(i);
    out.assign(set_x.begin(), set_x.end());
    std::sort(out.begin(), out.end());
    return out;
}

// function that splits string by commas and plus sign
inline std::vector<std::string> split_string(
    const std::string& x,
    const int& split_plus = 0
    )
{
    std::vector<std::string> out;
    size_t pos {0};
    if (x.length() == 0) {
        out.push_back("");
        return out;
    }
    for (size_t i {0}; i < x.length(); ) {
        if (split_plus) {
            pos = std::min(x.find(',', i), x.find('+', i));
        } else {
            pos = x.find(',', i);
        }
        out.push_back(x.substr(i, pos - i));
        if (pos == std::string::npos) break;
        i = pos + 1;
    }
    return out;
}

// rcpp version of the strsplit function with split = "," in R
// [[Rcpp::export]]
Rcpp::CharacterVector rcpp_split_string(const Rcpp::CharacterVector& x)
{
    if (Rcpp::CharacterVector::is_na(x[0])) {
        return NA_STRING;
    } else {
        std::vector<std::string> xx {
            split_string(Rcpp::as<std::string>(x[0]), 0) };
        Rcpp::CharacterVector out(xx.size());
        // implicit conversion
        out = xx;
        return out;
    }
}

// [[Rcpp::export]]
Rcpp::List rcpp_strsplit(
    const Rcpp::CharacterVector& x)
{
    return Rcpp::lapply(x, rcpp_split_string);
}


// function that concatenates strings by commas
inline std::string cat_string(const std::vector<std::string>& x)
{
    std::string out { x[0] };
    for (size_t i {1}; i < x.size() ; ++i) {
        if (x[i].length() > 0) {
            if (out.length() > 0) {
                out += "," + x[i];
            } else {
                out = x[i];
            }
        }
    }
    return out;
}
// concatenate by commas
std::string cat_dx(const std::vector<std::string>& x)
{
    std::vector<std::string> xVec, tmp;
    for (size_t i {0}; i < x.size(); ++i) {
        tmp = split_string(x[i]);
        xVec.insert(xVec.end(), tmp.begin(), tmp.end());
    }
    xVec = uni_sort(xVec);
    return cat_string(xVec);
}

// element-wise concatenate by commas for two vectors
std::vector<std::string> cat_dx_pair(
    const std::vector<std::string>& a,
    const std::vector<std::string>& b
    )
{
    // a and b should have the same length
    if (a.size() != b.size())
        throw std::length_error("The input vectors must have same length.");
    std::vector<std::string> out;
    std::string tmp;
    for (size_t i {0}; i < a.size(); ++i) {
        if (a[i].length() == 0 || b[i].length() == 0) {
            tmp = a[i] + b[i];
            if (a[i].length() == 0 && b[i].length() == 0) {
                out.push_back("");
                continue;
            }
        } else {
            tmp = a[i] + "," + b[i];
        }
        out.push_back(cat_dx(split_string(tmp)));
    }
    return out;
}

// rcpp version of the strsplit function with split = "," in R
inline Rcpp::String rcpp_cat_string(const Rcpp::CharacterVector& x)
{
    if (Rcpp::CharacterVector::is_na(x[0])) {
        return NA_STRING;
    } else {
        std::string xx {
            cat_string(Rcpp::as<std::vector<std::string>>(x)) };
        Rcpp::String out {xx};
        return out;
    }
}
// [[Rcpp::export]]
Rcpp::CharacterVector rcpp_strcat(const Rcpp::List& x)
{
    return Rcpp::sapply(x, rcpp_cat_string);
}


// cache mappings
// =============================================================================
// initialize forward + reverse-backward mappings
void cache_frb_map(const int& year)
{
    gem f_gem, rb_gem;
    gem* p_gem;
    switch(year)
    {
        case 2017:
            f_gem = forward_map_2017;
            rb_gem = reverse_backward_map_2017;
            p_gem = &frb_map_2017;
            break;
        case 2018:
            f_gem = forward_map_2018;
            rb_gem = reverse_backward_map_2018;
            p_gem = &frb_map_2018;
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    std::vector<std::string> all_keys, values;
    for (gem_it i { f_gem.begin() }; i != f_gem.end(); ++i) {
        all_keys.push_back(i->first);
    }
    for (gem_it i { rb_gem.begin() }; i != rb_gem.end(); ++i) {
        all_keys.push_back(i->first);
    }
    all_keys = uni_sort(all_keys);
    for (std::vector<std::string>::iterator it { all_keys.begin() };
         it != all_keys.end(); ++it) {
        values.clear();
        values.push_back(f_gem[*it]);
        values.push_back(rb_gem[*it]);
        (*p_gem).insert(std::make_pair(*it, cat_dx(values)));
    }
}

// initialize backward + reverse-forward mappings
void cache_brf_map(const int& year)
{
    gem b_gem, rf_gem;
    gem* p_gem;
    switch(year)
    {
        case 2017:
            b_gem = backward_map_2017;
            rf_gem = reverse_forward_map_2017;
            p_gem = &brf_map_2017;
            break;
        case 2018:
            b_gem = backward_map_2018;
            rf_gem = reverse_forward_map_2018;
            p_gem = &brf_map_2018;
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    std::vector<std::string> all_keys, values;
    for (gem_it i { b_gem.begin() };
         i != b_gem.end(); ++i) {
        all_keys.push_back(i->first);
    }
    for (gem_it i { rf_gem.begin() };
         i != rf_gem.end(); ++i) {
        all_keys.push_back(i->first);
    }
    all_keys = uni_sort(all_keys);
    for (std::vector<std::string>::iterator it { all_keys.begin() };
         it != all_keys.end(); ++it) {
        values.clear();
        values.push_back(b_gem[*it]);
        values.push_back(rf_gem[*it]);
        (*p_gem).insert(std::make_pair(*it, cat_dx(values)));
    }
}


// not-so-real function templates
// for mapping one to many (scalar version)
inline std::string gem_o2m_scalar(
    const std::string& dx,
    gem& gem_map
    )
{
    return gem_map[dx];
}
// for mapping one to many (vector version)
inline std::vector<std::string> gem_o2m(
    const std::vector<std::string>& dx,
    gem& gem_map
    )
{
    std::vector<std::string> out;
    for (std::string i : dx) {
        out.push_back(gem_o2m_scalar(i, gem_map));
    }
    return out;
}
// for mapping many to many (scalar version)
inline std::string gem_m2m_scalar(
    const std::string& dx,
    gem& gem_map
    )
{
    std::string out;
    if (dx.find(',') != std::string::npos ||
        dx.find('+') != std::string::npos) {
        out = cat_dx(gem_o2m(split_string(dx, 1), gem_map));
    } else {
        out = gem_o2m_scalar(dx, gem_map);
    }
    return out;
}
// for mapping many to many (vector version)
inline std::vector<std::string> gem_m2m(
    const std::vector<std::string>& dx,
    gem& gem_map
    )
{
    std::vector<std::string> out;
    for (std::string i : dx) {
        out.push_back(gem_m2m_scalar(i, gem_map));
    }
    return out;
}

// helper for forward-reverse-backward mapping
std::vector<std::string> gem_frb(
    const std::vector<std::string>& dx,
    const int& which_year,
    bool cache = true
    )
{
    std::vector<std::string> out;
    gem *p_f_gem, *p_rb_gem, *p_frb_gem;
    switch(which_year)
    {
        case 2017:
            p_f_gem = &forward_map_2017;
            p_rb_gem = &reverse_backward_map_2017;
            p_frb_gem = &frb_map_2017;
            break;
        case 2018:
            p_f_gem = &forward_map_2018;
            p_rb_gem = &reverse_backward_map_2018;
            p_frb_gem = &frb_map_2018;
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    // main part
    if (cache) {
        if (p_frb_gem->empty())
            cache_frb_map(which_year);
        out = gem_m2m(dx, *p_frb_gem);
    } else {
        out = cat_dx_pair(gem_m2m(dx, *p_f_gem),
                          gem_m2m(dx, *p_rb_gem));
    }
    return out;
}

// helper for backward_reverse-forward mapping
std::vector<std::string> gem_brf(
    const std::vector<std::string>& dx,
    const int& which_year,
    bool cache = true
    )
{
    std::vector<std::string> out;
    gem *p_b_gem, *p_rf_gem, *p_brf_gem;
    // switch by year
    switch(which_year)
    {
        case 2017:
            p_b_gem = &backward_map_2017;
            p_rf_gem = &reverse_forward_map_2017;
            p_brf_gem = &brf_map_2017;
            break;
        case 2018:
            p_b_gem = &backward_map_2018;
            p_rf_gem = &reverse_forward_map_2018;
            p_brf_gem = &brf_map_2018;
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    // main part
    if (cache) {
        if (p_brf_gem->empty())
            cache_brf_map(which_year);
        out = gem_m2m(dx, *p_brf_gem);
    } else {
        out = cat_dx_pair(gem_m2m(dx, *p_b_gem),
                          gem_m2m(dx, *p_rf_gem));
    }
    return out;
}


// cache multiple-stage mappings from version 9 to version 10
void cache_multi_0910_map(const int& which_year)
{
    gem *p_frb_gem, *p_brf_gem, *p_gem;
    switch(which_year)
    {
        case 2017:
            // cache first if needed
            p_frb_gem = &frb_map_2017;
            p_brf_gem = &brf_map_2017;
            p_gem = &multi_0910_map_2017;
            break;
        case 2018:
            p_frb_gem = &frb_map_2018;
            p_brf_gem = &brf_map_2018;
            p_gem = &multi_0910_map_2018;
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    // initialize maps if needed
    if (p_frb_gem->empty()) cache_frb_map(which_year);
    if (p_brf_gem->empty()) cache_brf_map(which_year);
    std::string key, value;
    for (gem_it it { p_frb_gem->begin() }; it != p_frb_gem->end(); ++it) {
        key = it->first;
        value = it->second;
        value = gem_m2m_scalar(value, *p_brf_gem);
        value = gem_m2m_scalar(value, *p_frb_gem);
        p_gem->insert(std::make_pair(key, value));
    }
}

// cache multiple-stage mappings from version 10 to version 9
void cache_multi_1009_map(const int& which_year)
{
    gem *p_frb_gem, *p_brf_gem, *p_gem;
    switch(which_year)
    {
        case 2017:
            p_frb_gem = &frb_map_2017;
            p_brf_gem = &brf_map_2017;
            p_gem = &multi_1009_map_2017;
            break;
        case 2018:
            p_frb_gem = &frb_map_2018;
            p_brf_gem = &brf_map_2018;
            p_gem = &multi_1009_map_2018;
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    // initialze maps if needed
    if (p_frb_gem->empty()) cache_frb_map(which_year);
    if (p_brf_gem->empty()) cache_brf_map(which_year);
    std::string key, value;
    for (gem_it it {p_brf_gem->begin()}; it != p_brf_gem->end(); ++it) {
        key = it->first;
        value = it->second;
        value = gem_m2m_scalar(value, *p_frb_gem);
        value = gem_m2m_scalar(value, *p_brf_gem);
        p_gem->insert(std::make_pair(key, value));
    }
}


// helper for multi-stage mapping from version 9 to 10
std::vector<std::string> gem_0910_multi(
    const std::vector<std::string>& dx,
    const int& which_year,
    bool cache = true
    )
{
    std::vector<std::string> out;
    gem *p_gem;
    switch(which_year)
    {
        case 2017:
            p_gem = &multi_0910_map_2017;
            break;
        case 2018:
            p_gem = &multi_0910_map_2018;
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    if (cache) {
        if (p_gem->empty())
            cache_multi_0910_map(which_year);
        out = gem_m2m(dx, *p_gem);
    } else {
        out = gem_frb(gem_brf(gem_frb(dx, which_year), which_year), which_year);
    }
    return out;
}

// helper for multi-stage mapping from version 10 to 9
std::vector<std::string> gem_1009_multi(
    const std::vector<std::string>& dx,
    const int& which_year,
    bool cache = true
    )
{
    std::vector<std::string> out;
    gem *p_gem;
    switch(which_year)
    {
        case 2017:
            p_gem = &multi_1009_map_2017;
            break;
        case 2018:
            p_gem = &multi_1009_map_2018;
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    if (cache) {
        if (p_gem->empty())
            cache_multi_1009_map(which_year);
        out = gem_m2m(dx, *p_gem);
    } else {
        out = gem_brf(gem_frb(gem_brf(dx, which_year), which_year), which_year);
    }
    return out;
}


// the engine function
// [[Rcpp::export]]
std::vector<std::string> rcpp_gem(
    const std::vector<std::string>& dx,
    const int& which_map,
    bool cache = true
    )
{
    std::vector<std::string> out;
    switch(which_map)
    {
        case 1709101:
            // 2017, 9 to 10, forward
            out = gem_m2m(dx, forward_map_2017);
            break;

        case 1709102:
            // 2017, 9 to 10, reverse-backward
            out = gem_m2m(dx, reverse_backward_map_2017);
            break;

        case 1709103:
            // 2017, 9 to 10, forward + reverse-backward
            out = gem_frb(dx, 2017, cache);
            break;

        case 1709104:
            // 2017, 9 to 10, 3 stages
            out = gem_0910_multi(dx, 2017, cache);
            break;

        case 1710091:
            // 2017, 10 to 9, backward
            out = gem_m2m(dx, forward_map_2017);
            break;

        case 1710092:
            // 2017, 10 to 9, reverse-forward
            out = gem_m2m(dx, reverse_forward_map_2017);
            break;

        case 1710093:
            // 2017, 10 to 9, backward + reverse-forward
            out = gem_brf(dx, 2017, cache);
            break;

        case 1710094:
            // 2017, 9 to 10, 3 stages
            out = gem_1009_multi(dx, 2017, cache);
            break;

        case 1809101:
            // 2018, 9 to 10, forward
            out = gem_m2m(dx, forward_map_2018);
            break;

        case 1809102:
            // 2018, 9 to 10, reverse-backward
            out = gem_m2m(dx, reverse_backward_map_2018);
            break;

        case 1809103:
            // 2018, 9 to 10, forward + reverse-backward
            out = gem_frb(dx, 2018, cache);
            break;

        case 1809104:
            // 2018, 9 to 10, 3 stages
            out = gem_0910_multi(dx, 2018, cache);
            break;

        case 1810091:
            // 2018, 10 to 9, backward
            out = gem_m2m(dx, backward_map_2018);
            break;

        case 1810092:
            // 2018, 10 to 9, reverse-forward
            out = gem_m2m(dx, reverse_forward_map_2018);
            break;

        case 1810093:
            // 2018, 10 to 9, backward + reverse-forward
            out = gem_brf(dx, 2018, cache);
            break;

        case 1810094:
            // 2018, 9 to 10, 3 stages
            out = gem_1009_multi(dx, 2018, cache);
            break;

        default:
            // otherwise, throw error
            throw std::range_error("Map cannot be found.");
    }
    return out;
}
