FROM python:3
WORKDIR /build/docker_app

COPY ./dist ./dist

RUN pip install dist/nested_fit*.whl

CMD [""]
