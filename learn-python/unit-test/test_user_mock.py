from unittest.mock import patch

from user import User


@patch('requests.get')
def test_add_bookmark(mock_requests_get):
    def func(link):
        class Response:
            pass

        resp = Response()
        if link == 'http://valid/link':
            resp.status_code = 200
        else:
            resp.status_code = 404
        return resp

    mock_requests_get.side_effect = func
    user = User(username='aptg', password='hahaha')
    assert user.add_bookmark('http://valid/link')
    assert not user.add_bookmark('http://invalid/link')
