# Load required libraries
library(keras)
library(tensorflow)
library(dplyr)
library(ggplot2)
library(scales)

dat_raw = read.csv("Electric_Production.csv")

dat_ts = dat_raw |>
  mutate(DATE = mdy(DATE)) |>
  rename(time = DATE) |>
  rename(value = Value) |>
  ts_ts()

data = dat_raw |>
  pull(Value)

time = 1:length(data)

# Set random seed for reproducibility
set.seed(42)
tensorflow::set_random_seed(42)

# Generate synthetic time series data
time <- 1:100
data <- sin(time * 0.1) + rnorm(length(time), sd = 0.1)

# Normalize the data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
data_scaled <- normalize(data)

# Function to create sequences
create_sequences <- function(data, seq_length) {
  X <- list()
  y <- c()
  
  for (i in seq_len(length(data) - seq_length)) {
    X[[i]] <- data[i:(i + seq_length - 1)]
    y <- c(y, data[i + seq_length])
  }
  
  X_array <- array(unlist(X), dim = c(length(X), seq_length, 1))
  y_array <- array(y, dim = c(length(y), 1))
  
  return(list(X = X_array, y = y_array))
}

# Define sequence length
seq_length <- 10
train_size <- floor(0.8 * length(data_scaled))

# Create sequences
seq_data <- create_sequences(data_scaled, seq_length)
X <- seq_data$X
y <- seq_data$y

# Split into training and testing sets
X_train <- X[1:train_size, , , drop = FALSE]
y_train <- y[1:train_size]
X_test <- X[(train_size + 1):length(y), , , drop = FALSE]
y_test <- y[(train_size + 1):length(y)]

# Build LSTM Model
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, activation = "relu", return_sequences = FALSE, input_shape = c(seq_length, 1)) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dense(units = 1)  # Single output

# Compile model with Adam optimizer
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse"
)

# Train the model
history <- model %>% fit(
  X_train, y_train,
  epochs = 50,
  batch_size = 16,
  validation_data = list(X_test, y_test),
  verbose = 1
)

# Make predictions
y_pred <- model %>% predict(X_test)

# Rescale predictions back to original range
rescale <- function(x, orig) {
  return(x * (max(orig) - min(orig)) + min(orig))
}
y_test_rescaled <- rescale(y_test, data)
y_pred_rescaled <- rescale(y_pred, data)

df_results_old = df_results

# Plot results
df_results <- data.frame(
  Time = time[(train_size + seq_length + 1):length(time)],
  Actual = y_test_rescaled,
  Predicted = y_pred_rescaled
)

ggplot(df_results, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "LSTM Time Series Forecasting with Adam Optimizer",
       x = "Time", y = "Value") +
  theme_minimal()
