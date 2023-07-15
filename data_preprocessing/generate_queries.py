# generate queries that I want
import yaml

#UNDERREP_less25	UNDERREP_25_to_34	UNDERREP_35_to_49	UNDERREP_50_to_74	UNDERREP_75_to_99	UNDERREP_100_to_149	UNDERREP_150_to_199	UNDERREP_200_more

def make_query(type_query:str, query_values:dict, subpop:str):
    query = {
        "type":type_query,
        "survey_query":{"query":query_values},
        "surveillance_query":{"query":None, "subpop":subpop, "target_pop":None}
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
    gender = ["Male", "Female"]
    subpop_gender_levels = [race+"_FEMALE", race+"_MALE"]
    return gender, subpop_gender_levels

def get_school_levels(race):

    # HISP_LESS_HS_TOTAL	HISP_HS_TOTAL	HISP_SOME_COLLEGE_TOTAL	HISP_COLLEGE_TOTAL

    school_levels = ["LESS_THAN_HS", "HS", 
                     "SOME_COLLEGE", "COLLEGE"] # possible need to change
    subpop_school_levels = [race+"_LESS_HS_TOTAL", race+"_HS_TOTAL", 
                            race+"_SOME_COLLEGE_TOTAL", race+"_COLLEGE_TOTAL"]

    return school_levels, subpop_school_levels

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
        all_queries["query{}".format(query_num)] = make_query(tq, survey_query, subpop_values)
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
        all_queries["query{}".format(query_num)] = make_query(tq, survey_query, subpop_values)
        query_num += 1

assert query_num == 20*2

to_save = "/Users/christianvarner/Research/ccs-knowledge-map/data_preprocessing/queries/generated_queries.yaml"
with open(to_save, "w") as f:
    yaml.dump(all_queries, f)