@authed
Feature: Users resource
  In order to authenticate access to the queue
  API users should be able to read, add and remove queue users

  Background:
    Given these existing users:
      | kevin |
      | mark  |
      | sean  |

  Scenario: List users
    When I GET the "/users" resource
    Then I should get a 200 response code
    And the exploded response should be an array

  Scenario: Find a user
    When I GET the "/users/kevin" resource
    Then I should get a 200 response code
    And the exploded response should be a string

  Scenario: Create a user
    When I PUT the "/users/dawn" resource with "'foobar'"
    Then I should get a 201 response code
    And the response body should be empty
    And the "Location" header should contain "/users/dawn"

  Scenario: Update a user
    When I PUT the "/users/kevin" resource with "'foobar'"
    Then I should get a 204 response code
    And the response body should be empty

  Scenario: Delete a user
    When I DELETE the "/users/kevin" resource
    Then I should get a 204 response code
    And the response body should be empty