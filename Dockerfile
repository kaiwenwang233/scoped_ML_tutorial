# FROM ghcr.io/seisscoped/container-base:ubuntu22.04_jupyterlab
FROM python:3.7


# Prevents Python from writing pyc files.
ENV PYTHONDONTWRITEBYTECODE=1

# Keeps Python from buffering stdout and stderr to avoid situations where
# the application crashes without emitting any logs due to buffering.
ENV PYTHONUNBUFFERED=1

WORKDIR /app

# Create a non-privileged user that the app will run under.
# See https://docs.docker.com/go/dockerfile-user-best-practices/
# ARG UID=10001
# RUN adduser \
#     --disabled-password \
#     --gecos "" \
#     --home "/nonexistent" \
#     --shell "/sbin/nologin" \
#     --no-create-home \
#     --uid "${UID}" \
#     appuser

RUN pip install git+https://github.com/kaiwenwang233/scoped_tutorial.git


COPY requirements.txt /app
RUN python -m pip install -r requirements.txt

RUN pip install git+https://github.com/wayneweiqiang/GaMMA.git


# Copy the source code into the container.
COPY . ./app

# Expose the port that the application listens on.
EXPOSE 8888

# # Run the notebook.
CMD ["jupyter", "notebook", "--port=8888", "--ip=0.0.0.0", "--allow-root"]