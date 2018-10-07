// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <string>
#include <vector>
#include <algorithm>

#include <sparsepp/spp.h>
//[[Rcpp::depends(sparsepp)]]

typedef spp::sparse_hash_map<std::string, std::string> gem;


// gem for year 2017
extern gem forward_map_2017;
extern gem backward_map_2017;
extern gem reverse_forward_map_2017;
extern gem reverse_backward_map_2017;

// gem for year 2018
extern gem forward_map_2018;
extern gem backward_map_2018;
extern gem reverse_forward_map_2018;
extern gem reverse_backward_map_2018;


// function that splits string by commas
inline std::vector<std::string> split_string(const std::string& x)
{
    std::vector<std::string> out;
    size_t pos {0};
    if (x.length() == 0) {
        out.push_back("");
        return out;
    }
    for (size_t i {0}; i < x.length(); ) {
        pos = x.find(',', i);
        out.push_back(x.substr(i, pos - i));
        if (pos == std::string::npos) break;
        i = pos + 1;
    }
    return out;
}

// rcpp version of the strsplit function with split = "," in R
inline Rcpp::CharacterVector rcpp_split_string(const Rcpp::CharacterVector& x)
{
    if (Rcpp::CharacterVector::is_na(x[0])) {
        return NA_STRING;
    } else {
        std::vector<std::string> xx {
            split_string(Rcpp::as<std::string>(x[0])) };
        Rcpp::CharacterVector out(xx.size());
        // implicit conversion
        out = xx;
        return out;
    }
}
// [[Rcpp::export]]
Rcpp::List rcpp_strsplit(const Rcpp::CharacterVector& x)
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

// concatenate by commas
// [[Rcpp::export]]
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
// [[Rcpp::export]]
std::vector<std::string> cat_dx_pair(
    const std::vector<std::string>& a,
    const std::vector<std::string>& b
    )
{
    // a and b should have the same length
    // TODO: add a simple test here
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


// two not-so-real function templates
// for mapping one to many
inline std::vector<std::string> gem_o2m(
    const std::vector<std::string>& dx,
    gem& gem_map
    )
{
    std::vector<std::string> out;
    for (std::string i : dx) {
        out.push_back(gem_map[i]);
    }
    return out;
}
// for mapping many to many
inline std::vector<std::string> gem_m2m(
    const std::vector<std::string>& dx,
    gem& gem_map
    )
{
    std::vector<std::string> out;
    for (std::string i : dx) {
        out.push_back(cat_dx(gem_o2m(split_string(i), gem_map)));
    }
    return out;
}

// helper for forward-reverse-backward mapping
// one to many
std::vector<std::string> gem_frb_o2m(
    const std::vector<std::string>& dx,
    const int& which_year
    )
{
    std::vector<std::string> out;
    switch(which_year)
    {
        case 2017:
            out = cat_dx_pair(gem_o2m(dx, forward_map_2017),
                              gem_o2m(dx, reverse_backward_map_2017));
            break;
        case 2018:
            out = cat_dx_pair(gem_o2m(dx, forward_map_2018),
                              gem_o2m(dx, reverse_backward_map_2018));
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    return out;
}
// many to many
std::vector<std::string> gem_frb_m2m(
    const std::vector<std::string>& dx,
    const int& which_year
    )
{
    std::vector<std::string> out;
    switch(which_year)
    {
        case 2017:
            out = cat_dx_pair(gem_m2m(dx, forward_map_2017),
                              gem_m2m(dx, reverse_backward_map_2017));
            break;
        case 2018:
            out = cat_dx_pair(gem_m2m(dx, forward_map_2018),
                              gem_m2m(dx, reverse_backward_map_2018));
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    return out;
}

// helper for backward_reverse-forward mapping
// one to many
std::vector<std::string> gem_brf_o2m(
    const std::vector<std::string>& dx,
    const int& which_year
    )
{
    std::vector<std::string> out;
    switch(which_year)
    {
        case 2017:
            out = cat_dx_pair(gem_o2m(dx, backward_map_2017),
                              gem_o2m(dx, reverse_forward_map_2017));
            break;
        case 2018:
            out = cat_dx_pair(gem_o2m(dx, backward_map_2018),
                              gem_o2m(dx, reverse_forward_map_2018));
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    return out;
}
// many to many
std::vector<std::string> gem_brf_m2m(
    const std::vector<std::string>& dx,
    const int& which_year
    )
{
    std::vector<std::string> out;
    switch(which_year)
    {
        case 2017:
            out = cat_dx_pair(gem_m2m(dx, backward_map_2017),
                              gem_m2m(dx, reverse_forward_map_2017));
            break;
        case 2018:
            out = cat_dx_pair(gem_m2m(dx, backward_map_2018),
                              gem_m2m(dx, reverse_forward_map_2018));
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    return out;
}


// helper for multi-stage mapping from version 9 to 10
// one to many
std::vector<std::string> gem_0910_multi_o2m(
    const std::vector<std::string>& dx,
    const int& which_year
    )
{
    std::vector<std::string> out;
    switch(which_year)
    {
        case 2017:
            out = gem_frb_m2m(gem_brf_m2m(gem_frb_o2m(dx, 2017), 2017), 2017);
            break;
        case 2018:
            out = gem_frb_m2m(gem_brf_m2m(gem_frb_o2m(dx, 2018), 2018), 2018);
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    return out;
}
// many to many
std::vector<std::string> gem_0910_multi_m2m(
    const std::vector<std::string>& dx,
    const int& which_year
    )
{
    std::vector<std::string> out;
    switch(which_year)
    {
        case 2017:
            out = gem_frb_m2m(gem_brf_m2m(gem_frb_m2m(dx, 2017), 2017), 2017);
            break;
        case 2018:
            out = gem_frb_m2m(gem_brf_m2m(gem_frb_m2m(dx, 2018), 2018), 2018);
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    return out;
}

// helper for multi-stage mapping from version 10 to 9
// one to many
std::vector<std::string> gem_1009_multi_o2m(
    const std::vector<std::string>& dx,
    const int& which_year
    )
{
    std::vector<std::string> out;
    switch(which_year)
    {
        case 2017:
            out = gem_brf_m2m(gem_frb_m2m(gem_brf_o2m(dx, 2017), 2017), 2017);
            break;
        case 2018:
            out = gem_brf_m2m(gem_frb_m2m(gem_brf_o2m(dx, 2018), 2018), 2018);
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    return out;
}
// many to many
std::vector<std::string> gem_1009_multi_m2m(
    const std::vector<std::string>& dx,
    const int& which_year
    )
{
    std::vector<std::string> out;
    switch(which_year)
    {
        case 2017:
            out = gem_frb_m2m(gem_brf_m2m(gem_frb_m2m(dx, 2017), 2017), 2017);
            break;
        case 2018:
            out = gem_frb_m2m(gem_brf_m2m(gem_frb_m2m(dx, 2018), 2018), 2018);
            break;
        default:
            throw std::range_error("Year cannot be found.");
    }
    return out;
}


// one-to-many code mappings
// [[Rcpp::export]]
std::vector<std::string> rcpp_gem_o2m(
    const std::vector<std::string>& dx,
    const int& which_map
    )
{
    std::vector<std::string> out;
    switch(which_map)
    {
        case 1709101:
            // 2017, 9 to 10, forward
            out = gem_o2m(dx, forward_map_2017);
            break;

        case 1709102:
            // 2017, 9 to 10, reverse-backward
            out = gem_o2m(dx, reverse_backward_map_2017);
            break;

        case 1709103:
            // 2017, 9 to 10, forward + reverse-backward
            out = gem_frb_o2m(dx, 2017);
            break;

        case 1709104:
            // 2017, 9 to 10, 3 stages
            out = gem_0910_multi_o2m(dx, 2017);
            break;

        case 1710091:
            // 2017, 10 to 9, backward
            out = gem_o2m(dx, forward_map_2017);
            break;

        case 1710092:
            // 2017, 10 to 9, reverse-forward
            out = gem_o2m(dx, reverse_forward_map_2017);
            break;

        case 1710093:
            // 2017, 10 to 9, backward + reverse-forward
            out = gem_brf_o2m(dx, 2017);
            break;

        case 1710094:
            // 2017, 9 to 10, 3 stages
            out = gem_1009_multi_o2m(dx, 2017);
            break;

        case 1809101:
            // 2018, 9 to 10, forward
            out = gem_o2m(dx, forward_map_2018);
            break;

        case 1809102:
            // 2018, 9 to 10, reverse-backward
            out = gem_o2m(dx, reverse_backward_map_2018);
            break;

        case 1809103:
            // 2018, 9 to 10, forward + reverse-backward
            out = gem_frb_o2m(dx, 2018);
            break;

        case 1809104:
            // 2018, 9 to 10, 3 stages
            out = gem_0910_multi_o2m(dx, 2018);
            break;

        case 1810091:
            // 2018, 10 to 9, backward
            out = gem_o2m(dx, backward_map_2018);
            break;

        case 1810092:
            // 2018, 10 to 9, reverse-forward
            out = gem_o2m(dx, reverse_forward_map_2018);
            break;

        case 1810093:
            // 2018, 10 to 9, backward + reverse-forward
            out = gem_brf_o2m(dx, 2018);
            break;

        case 1810094:
            // 2018, 9 to 10, 3 stages
            out = gem_1009_multi_o2m(dx, 2018);
            break;

        default:
            // otherwise, throw error
            throw std::range_error("Map cannot be found.");
    }
    return out;
}

// many-to-many code mappings
// [[Rcpp::export]]
std::vector<std::string> rcpp_gem_m2m(
    const std::vector<std::string>& dx,
    const int& which_map
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
            out = gem_frb_m2m(dx, 2017);
            break;

        case 1709104:
            // 2017, 9 to 10, 3 stages
            out = gem_0910_multi_m2m(dx, 2017);
            break;

        case 1710091:
            // 2017, 10 to 9, backward
            out = gem_m2m(dx, backward_map_2017);
            break;

        case 1710092:
            // 2017, 10 to 9, reverse-forward
            out = gem_m2m(dx, reverse_forward_map_2017);
            break;

        case 1710093:
            // 2017, 10 to 9, backward + reverse-forward
            out = gem_brf_m2m(dx, 2017);
            break;

        case 1710094:
            // 2017, 9 to 10, 3 stages
            out = gem_1009_multi_m2m(dx, 2017);
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
            out = gem_frb_m2m(dx, 2018);
            break;

        case 1809104:
            // 2018, 9 to 10, 3 stages
            out = gem_0910_multi_m2m(dx, 2018);
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
            out = gem_brf_m2m(dx, 2018);
            break;

        case 1810094:
            // 2018, 9 to 10, 3 stages
            out = gem_1009_multi_m2m(dx, 2018);
            break;

        default:
            // otherwise, throw error
            throw std::range_error("Map cannot be found.");
    }
    return out;
}
