@simplepass
Feature: Digest authentication
  In order to provide a modicum of security
  API users should have to use HTTP Digest Authentication

  Scenario: Unauthenticated
    Given I have not provided authentication credentials
    When I request the "/vhosts" resource
    Then I should get a 401 response code
    And I should get a "WWW-Authenticate" header containing "Digest"

  Scenario: Incorrect authentication
    Given I have provided the authentication credentials "foobar"/"baz"
    When I request the "/vhosts" resource
    Then I should get a 401 response code
    And I should get a "WWW-Authenticate" header containing "Digest"

  Scenario: Correct authentication
    Given I have provided the authentication credentials "foobar"/"foobar"
    When I request the "/vhosts" resource
    Then I should get a 200 response code