# coding=utf-8
"""Blog feature tests."""
import pytest
from pytest_bdd import (
    given,
    scenario,
    then,
    when,
)

"""
Order of calls -
author
article
browser
i_go_to_the_article_page
i_press_the_publish_button
i_should_not_see_the_error_message
the_article_should_be_published
scenario
"""


@pytest.fixture
def browser():
    print('browser')
    return {
        'address': None,
        'button': False,
        'dom': None
    }


@scenario('features/simple_scratch.feature', 'Publishing the article')
def test_publishing_the_article():
    print('scenario')


@given("I'm an author user")
def author():
    print('author')
    return {'name': 'Happy Orange'}


@given('I have an article')
def article(author):
    print('article')
    article = {
        'url': 'http://url/to/my/article',
        'contents': 'Lorem ipsum dolor set'
    }
    author['article'] = article
    return article


@when('I go to the article page')
def i_go_to_the_article_page(browser, article):
    print('i_go_to_the_article_page')
    browser['address'] = article['url']


@when('I press the publish button')
def i_press_the_publish_button(browser):
    print('i_press_the_publish_button')
    browser['button'] = True
    browser['dom'] = 'some dom here'


@then('I should not see the error message')
def i_should_not_see_the_error_message(browser):
    print('i_should_not_see_the_error_message')
    assert browser['dom'].find('error') == -1


@then('the article should be published')
def the_article_should_be_published(article):
    print('the_article_should_be_published')
    assert article is not None

