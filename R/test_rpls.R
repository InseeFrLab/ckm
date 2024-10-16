install.packages("aws.s3", repos = "https://cloud.R-project.org")

Sys.setenv("AWS_ACCESS_KEY_ID" = "2CBKRW6PHWCB4T6AHLAV",
           "AWS_SECRET_ACCESS_KEY" = "oUGSxUgnP9JyLq1Ayn69TLMaEZizjWolRJZ6qT8w",
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiIyQ0JLUlc2UEhXQ0I0VDZBSExBViIsImFjciI6IjAiLCJhdWQiOiJtaW5pby1rdWJlLWRhdGFzY2llbmNlIiwiYXV0aF90aW1lIjoxNzI5MTAzMjg2LCJhenAiOiJvbnl4aWEtbWluaW8ta3ViZS1kYXRhc2NpZW5jZSIsImVtYWlsIjoianVsaWVuLmphbW1lQGluc2VlLmZyIiwiZW1haWxfdmVyaWZpZWQiOmZhbHNlLCJleHAiOjE3Mjk1MzUzMzgsImZhbWlseV9uYW1lIjoiSmFtbWUiLCJnaXZlbl9uYW1lIjoiSnVsaWVuIiwiZ3JvdXBzIjpbIiJdLCJpYXQiOjE3MjkxMDMzMzgsImlzcyI6Imh0dHBzOi8vYXV0aC5pbnNlZS5mci9hdXRoL3JlYWxtcy9pbnNlZS1kYXRhc2NpZW5jZSIsImp0aSI6IjRmNmQyNDVlLTAyOWYtNDU5My1iOWZkLTJkNTU0NmY0MGQwYyIsIm5hbWUiOiJKdWxpZW4gSmFtbWUiLCJwb2xpY3kiOiJwdWJsaWMsdHJhdmFpbCIsInByZWZlcnJlZF91c2VybmFtZSI6InRtbTdhbiIsInNjb3BlIjoib3BlbmlkIHMzLWRhdGFzY2llbmNlLWdyb3Vwcy1yZWdleC1maWx0ZXItbmV3IGVtYWlsIHByb2ZpbGUgczMtZGF0YXNjaWVuY2UtcG9saWN5LXJlZ2V4LWZpbHRlci1uZXciLCJzZXNzaW9uX3N0YXRlIjoiYzhiNzhjZWItYWIxZi00ODM0LWI3NjYtN2RlZWFjNWNiZmZlIiwic2lkIjoiYzhiNzhjZWItYWIxZi00ODM0LWI3NjYtN2RlZWFjNWNiZmZlIiwic3ViIjoiZjoxOGMxZThkOS0yY2E0LTQ0ZGYtOGJlOC02NTlmZmY0NjQ4ZGI6VE1NN0FOIiwidHlwIjoiQmVhcmVyIn0.WAVrJFtjR2d841TefCaTlq_Qn1bavMHcZ7u52NHqxmOinbUof9k5ieZ_vKnZix7J-tCP-E23hlT5P6PFIPa7pw",
           "AWS_S3_ENDPOINT"= "minio.datascience.kube.insee.fr")

library("aws.s3")
bucketlist(region="")


library(arrow)

# rpls <-
#   aws.s3::s3read_using(
#     FUN = arrow::read_parquet,
#     # Mettre les options de FUN ici
#     object = "/confidentiel/personnel_sensible/Mad_repls21_rep_tot_g22_CVDL.parquet",
#     bucket = "travail/user-tmm7an",
#     opts = list("region" = "")
#   )

rpls <-
  aws.s3::s3read_using(
    FUN = readRDS,
    # Mettre les options de FUN ici
    object = "/confidentiel/personnel_sensible/rpls_2022_detail.rds",
    bucket = "travail/user-tmm7an",
    opts = list("region" = "")
  )

str(rpls)
rpls %>%
  group_by(PLG_QP) %>%
  summarise(
    nbLsTot	=	sum(	MODE!="9"	, na.rm=T)
  )


# Calculer les fréquences empiriques sur un indicateur
# Calculer le risque avec mesurer_risque()
