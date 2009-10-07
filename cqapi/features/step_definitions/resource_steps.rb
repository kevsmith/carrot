Before do
  @http = Patron::Session.new
  @http.base_url = API_URL
end