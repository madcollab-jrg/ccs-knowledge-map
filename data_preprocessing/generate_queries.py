# generate queries that I want
import yaml
import os

def make_query(type_query:str, query_values:dict, subpop:str, col:str, row:str):
    """
    Make one query that will be used to calculate the log disparity metric

    params:
        type_query - type of query, should income, education, gender, or age
        query_values - dictionary where keys are columns in the survey files 
                        and values are of such columns (selection of subpopulation in survey)
        subpop - string that is the column of the subpopulation in the census data

    return:
        dictionary that represents the query, the specific form is used by downstream application so don't change
    """
    query = {
        "type":type_query,
        "survey_query":{"query":query_values}, 
        "surveillance_query":{"query":None, "subpop":subpop, "target_pop":None},
        "col": col,
        "row": row
        }
    return query

def get_income_levels(race):
    income_levels = ["Less than $25,000", "$25,000 to $34,999", 
                     "$35,000 to $49,999", "$50,000 to $74,999", 
                     "$75,000 to $99,999", "$100,000 to $149,999",
                    "$150,000 to $199,999",  "$200,000 or more"]

    subpop_income_levels = ["_less25", "_25_to_34",
                            "_35_to_49","_50_to_74", 
                            "_75_to_99","_100_to_149", 
                            "_150_to_199", "_200_more"]

    for i,val in enumerate(subpop_income_levels):
        subpop_income_levels[i] = race+val
    
    return income_levels, subpop_income_levels

def get_age_levels(race):

    age_levels = ["18_to_24", "25_to_34", 
                         "35_to_44", "45_to_54",
                         "55_to_64", "65_over"]

    subpop_age_levels = ["_18_to_24", "_25_to_34", 
                         "_35_to_44", "_45_to_54",
                         "_55_to_64", "_65_over"]
    
    for i, val in enumerate(subpop_age_levels):
        subpop_age_levels[i] = race+val

    return  age_levels, subpop_age_levels

def get_gender_levels(race):
    gender = ["Female", "Male"]
    subpop_gender_levels = [race+"_FEMALE", race+"_MALE"]
    return gender, subpop_gender_levels

def get_school_levels(race):

    # HISP_LESS_HS_TOTAL	HISP_HS_TOTAL	HISP_SOME_COLLEGE_TOTAL	HISP_COLLEGE_TOTAL

    school_levels = ["LESS_THAN_HS", "HS", 
                     "SOME_COLLEGE", "COLLEGE"] # possible need to change
    subpop_school_levels = [race+"_LESS_HS_TOTAL", race+"_HS_TOTAL", 
                            race+"_SOME_COLLEGE_TOTAL", race+"_COLLEGE_TOTAL"]

    return school_levels, subpop_school_levels

# Generate the queries

if __name__ == "__main__":
    # black african american
    type_queries = ["income", "gender", "age", "education"]
    income_levels, subpop_income_levels = get_income_levels("BLACK")
    gender, subpop_gender_levels = get_gender_levels("BLACK")
    age, subpop_age_levels = get_age_levels("BLACK")
    edu, subpop_edu = get_school_levels("BLACK")
    type_query_column_map = {"income":"income_recode",
                            "age":"Year.of.Birth",
                            "gender":"Gender",
                            "education":"edu_recode"}

    query_info = {"income":income_levels, 
                "gender":gender, "age":age,
                "education":edu}
    subpop_info = {"income":subpop_income_levels, 
                "gender":subpop_gender_levels, "age":subpop_age_levels,
                "education":subpop_edu}

    query_num = 0
    all_queries = {}
    for tq in type_queries:

        for query_values, subpop_values in zip(query_info[tq], subpop_info[tq]):
            survey_query = {type_query_column_map[tq]:query_values, "race_recode":"Black or African American"}
            all_queries["query{}".format(query_num)] = make_query(tq, survey_query, subpop_values, "Black", query_values)
            query_num += 1

    # hispanic
    race = "HISP"
    income_levels, subpop_income_levels = get_income_levels(race)
    gender, subpop_gender_levels = get_gender_levels(race)
    age, subpop_age_levels = get_age_levels(race)
    edu, subpop_edu = get_school_levels(race)
    query_info = {"income":income_levels, 
                "gender":gender, "age":age,
                "education":edu}
    subpop_info = {"income":subpop_income_levels, 
                "gender":subpop_gender_levels, "age":subpop_age_levels,
                "education":subpop_edu}

    for tq in type_queries:
        for query_values, subpop_values in zip(query_info[tq], subpop_info[tq]):
            survey_query = {type_query_column_map[tq]:query_values, "hisp_code":"Hispanic"}
            all_queries["query{}".format(query_num)] = make_query(tq, survey_query, subpop_values, "Hispanice", query_values)
            query_num += 1

    assert query_num == 20*2

    to_save = os.path.join(os.getcwd(), "data_preprocessing", "queries", "generated_queries.yaml")
    with open(to_save, "w") as f:
        yaml.dump(all_queries, f)