package io.github.ssledz.app.ui.activities

import android.os.Bundle
import androidx.appcompat.app.AppCompatActivity
import io.github.ssledz.app.R
import kotlinx.android.synthetic.main.activity_main.*

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        message.text = "Hello Kotlin!"
    }
}
