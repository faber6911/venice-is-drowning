import keras
from matplotlib import pyplot as plt
from IPython.display import clear_output

class TrainingPlot(keras.callbacks.Callback):

    def on_train_begin(self, logs={}):
        self.losses = []
        self.mae = []
        self.mape = []
        self.val_losses = []
        self.val_mae = []
        self.val_mape = []
        #self.f1=[]
        #self.val_f1=[]
        self.logs = []

    def on_epoch_end(self, epoch, logs={}):
        #self.logs.append(logs)
        self.losses.append(logs.get('loss'))
        self.mae.append(logs.get('mean_absolute_error'))
        self.mape.append(logs.get('mean_absolute_percentage_error'))
        self.val_losses.append(logs.get('val_loss'))
        self.val_mae.append(logs.get('val_mean_absolute_error'))
        self.val_mape.append(logs.get('val_mean_absolute_percentage_error'))
        
        #self.f1.append(logs.get('f1'))
        #self.val_f1.append(logs.get('val_f1'))

        if len(self.losses)%1==0:

            clear_output(wait=True)
            N = np.arange(0, len(self.losses))

            #%matplotlib inline
            plt.style.use("seaborn")

            plt.subplots(figsize = (20,10))

            plt.subplot(1,2,1)
            plt.plot(N, self.losses, label = "train_mse")
            plt.plot(N, self.mae, label = "train_mae")
            
            plt.plot(N, self.val_losses, label = "val_mse")
            plt.plot(N, self.val_mae, label = "val_mae")
            #plt.plot(N, self.f1, label = "f1")
            #plt.plot(N, self.val_f1, label = "val_f1")
            #plt.ylim(0,0.5)
            plt.title("Training Loss and Accuracy [Epoch {}]".format(epoch))
            plt.xlabel("Epoch #")
            plt.ylabel("Loss(Mse)/Mae")
            plt.legend()
            #plt.show()
            
            plt.subplot(1,2,2)
            
            plt.style.use("seaborn")
            plt.plot(N, self.mape, label = "train_mape")
            plt.plot(N, self.val_mape, label = "val_mape")
            plt.title("Training Loss and Accuracy [Epoch {}]".format(epoch))
            plt.xlabel("Epoch #")
            plt.ylabel("Loss(Mse)/Mae")
            plt.legend()
            plt.show()