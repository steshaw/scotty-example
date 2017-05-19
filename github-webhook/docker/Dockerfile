from ubuntu:16.04

#
# curl to install Google Cloud SDK.
# python is a required dependency of Google Cloud SDK.
#
run apt-get -y update && apt-get -y install \
    curl \
    python

# Install Google Cloud SDK.
run curl -sS https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-153.0.0-linux-x86_64.tar.gz | tar -xzf - -C /opt
env PATH=/opt/google-cloud-sdk/bin:$PATH
env CLOUDSDK_CORE_DISABLE_PROMPTS=1
run gcloud components update && gcloud components install kubectl

# Install haskell-stack.
run curl -sSL https://get.haskellstack.org/ | sh
