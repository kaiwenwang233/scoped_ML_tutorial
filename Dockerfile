FROM ghcr.io/seisscoped/container-base:ubuntu22.04_jupyterlab
# FROM ghcr.io/seisscoped/container-base:centos7_jupyterlab
# FROM python:3.7


# Prevents Python from writing pyc files.
ENV PYTHONDONTWRITEBYTECODE=1

# Keeps Python from buffering stdout and stderr to avoid situations where
# the application crashes without emitting any logs due to buffering.
ENV PYTHONUNBUFFERED=1

WORKDIR /app


RUN pip install git+https://github.com/kaiwenwang233/scoped_ML_tutorial.git

# COPY requirements.txt /app

RUN pip install git+https://github.com/wayneweiqiang/GaMMA.git
COPY . ./app


RUN apt update -y && \
    apt install -y gcc gfortran gdb make && \
    pip install -r ./app/requirements.txt && \
    cd app/hypoInv/source && \
    make

# Expose the port that the application listens on.
EXPOSE 8888

WORKDIR /app
# # Run the notebook.
#CMD ["jupyter", "notebook", "--port=8888", "--ip=0.0.0.0", "--allow-root"]