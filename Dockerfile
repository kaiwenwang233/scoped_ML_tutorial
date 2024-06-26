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
COPY . /app


RUN apt update -y && \
    apt install -y gcc gfortran gdb make && \
    pip install -r /app/requirements.txt && \
    cd /app/hypoInv/source && \
    make

RUN pip install torch torchvision
# RUN pip install install torch==1.11.0+cpu torchvision==0.12.0+cpu -f https://download.pytorch.org/whl/torch_stable.html

RUN cd /app && \
    git clone http://github.com/fwaldhauser/HypoDD.git && \
    cd HypoDD && \
    # rm -r example.Amatrice/* && rmdir example.Amatrice &&\
    cd src && \
    make all && \
    cp ph2dt/ph2dt /usr/bin/ && \
    cp hypoDD/hypoDD /usr/bin/ && \
    cp hista2ddsta/hista2ddsta /usr/bin/ && \
    mv /app/HypoDD /app/hypodd/source 

RUN cd /app/cc/amatrice/preprocess/preprocess && \
    cp Makefile.Pwaves Makefile && \
    make && \
    cp Makefile.Swaves Makefile && \
    make && \
    cd /app/cc/amatrice/correl/correl && \
    cp Makefile.amatrice.Pwaves Makefile && \
    make && \
    cp Makefile.amatrice.Swaves Makefile && \
    make && \
    cd ../runs && \
    cc -o select5 select5.c -lm && \
    apt install csh

RUN cd /app/cc/convcc && \
    gfortran add_nphaPS.f -o add_nphaPS && \
    gfortran OTcorr.f -o OTcorr && \
    gfortran reformat_dtcc_sorted.f -o reformat_dtcc_sorted

USER root
# add all the stuff to change permissions
RUN mkdir ~/.local && \
    sudo chmod -R 777 ~/.local && \
    sudo chmod -R 777 /app

# Expose the port that the application listens on.
EXPOSE 8888

WORKDIR /app
# # Run the notebook.
CMD ["jupyter", "lab", "--port=8888", "--ip=0.0.0.0", "--allow-root"]

