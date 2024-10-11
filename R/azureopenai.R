#'AzureOpenAI_get_response: Get Chat Response from Azure OpenAI API
#'This function obtains response from Azure OpenAI API.
#'@param prompt Either a string or a list. When it is a string, the role 
#'      the prompt is assumed to be a user. If it is a list, it should be a
#'      list of list(role, prompt), such as
#'      list(list(role="system", content="You are a machine"),
#'           list(role="user",content="Hi, machine!")).
#'@param env A list of environment, which contains an API key and the endpoint URL:
#'      list(api_key="API_KEY", 
#'           endpoint="https://ENDPOINT/openai/deployments/MODELNAME/chat/completions?api-version=API_VERSION")
#'@param max_tokens Maximum number of tokens
#'@return A string that contains the response from the LLM
#'@export
#'@importFrom httr POST content
#'@importFrom jsonlite toJSON
#'@examples
#'api_key <- "API_KEY"
#'endpoint <- "https://ENDPOINT/openai/deployments/MODELNAME/chat/completions?api-version=API_VERSION"
#'environ <- list(
#'  api_key=api_key,
#'  endpoint=endpoint
#')
#'prompt <- "List 10 foods good for eat with coke"
#'resp <- AzureOpenAI_get_response(prompt,environ,max_tokens=1000)
#'print(resp)
AzureOpenAI_get_response <- function(prompt,env,role="user",max_tokens=100) {
  # Create the body of the POST request
  if (is.list(prompt)) {
    message <- prompt
  } else {
    message <- list(list(role=role,content=prompt))
  }
  body <- list(
    messages = message,
    max_tokens = max_tokens
  )
  
  # Make the POST request
  response <- POST(
    url = env$endpoint,
    add_headers(.headers = c("api-key" = env$api_key, 
                             "Content-Type" = "application/json")),
    body = toJSON(body,auto_unbox=TRUE),
    encode = "json"
  )
  
  # Parse the response
  content <- content(response, "parsed")
  content$choices[[1]]$message$content
}
