PORT=3000

curl --header "Content-Type: application/json" \
  --header "Accept: application/json" \
  --request POST \
  --data '{"first_name":"Mariusz","last_name":"Adamski","student_id":"090990"}' \
  http://localhost:${PORT}/student
